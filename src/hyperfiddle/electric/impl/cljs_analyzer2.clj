(ns hyperfiddle.electric.impl.cljs-analyzer2
  (:refer-clojure :exclude [find-var])
  (:require [edamame.core :as ed]
            [clojure.core :as cc]
            [clojure.string :as str]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.java.io :as io]
            [cljs.tagged-literals]
            [missionary.core :as m]
            [contrib.debug]
            [cljs.core]                 ; for cljs macroexpansion
            [cljs.env]
            [cljs.analyzer]
            [cljs.repl]))               ; for cljs macroexpansion

#_
(defn get-expander-ns [env ^String nstr]
  ;; first check for clojure.* -> cljs.* cases
  (let [res  (or (resolve-macro-ns-alias env nstr nil)
                 (resolve-ns-alias env nstr nil))
        nstr (if res (str res) nstr)]
    (cond (= "clojure.core" nstr) (find-ns 'cljs.core)
          (= "clojure.repl" nstr) (find-ns 'cljs.repl)
          (.contains nstr ".") (find-ns (symbol nstr))
          :else (some-> env :ns :require-macros (get (symbol nstr)) find-ns))))

(defn ns->basename [ns$] (-> ns$ name (.replace \- \_) (.replace \. \/)))

(defn ns->resource [ns$]
  (let [base (ns->basename ns$)]
    (or (io/resource (str base ".cljs"))
      (io/resource (str base ".cljc")))))

(let [parse-opts
      (ed/normalize-opts {:all true, :row-key :line, :col-key :column, :end-location false
                          :readers cljs.tagged-literals/*cljs-data-readers* :auto-resolve name
                          :features #{:cljs}, :read-cond :allow, :eof ::done})]
  (defn resource-forms> [rs]
    (->> (m/ap
           (let [rdr (m/?> (m/observe (fn [!]
                                        (let [rdr (rt/source-logging-push-back-reader (io/reader rs))]
                                          (! rdr) #(.close ^java.io.Reader rdr)))))]
             (m/? (m/?> (m/seed (repeat (m/sp (ed/parse-next rdr parse-opts))))))))
      (m/eduction (take-while (complement #{::done}))))))

(defn safe-require [sym]
  ;; we might be expanding clj code before the ns got loaded (during cljs compilation)
  ;; to correctly lookup vars the ns needs to be loaded
  ;; since shadow-cljs compiles in parallel we need to serialize the requires
  (when-not (get (loaded-libs) sym)
    (try (#'clojure.core/serialized-require sym) ; try bc it can be cljs file
         (catch java.io.FileNotFoundException _))))

(defn find-ns-var [^clojure.lang.Namespace nso sym] (.findInternedVar nso sym))
(declare find-var find-macro-var)

(defn mksym [& xs] (symbol (apply str (mapv #((if (or (keyword? %) (symbol? %)) name str) %) xs))))

(defmacro my-deftype [nm & _] `(do (def ~nm) (def ~(mksym '-> nm))))

(let [-base-cljs-env {:context :statement
                      :locals {}
                      :fn-scope []
                      :js-globals (into {}
                                    (map #(vector % {:op :js-var :name % :ns 'js})
                                      '(alert window document console escape unescape
                                         screen location navigator history location
                                         global process require module exports)))}]
  (defn ->cljs-env
    ([] (->cljs-env (ns-name *ns*)))
    ([nssym] (cond-> -base-cljs-env nssym (assoc :ns {:name nssym})))))

(def special? '#{if def fn* do let* loop* letfn* throw try catch finally
                 recur new set! ns deftype* defrecord* . js* & quote case* var ns*})

(let [blacklisted (into #{} (map cc/find-var)
                    '[cljs.core/exists? cljs.core/str cljs.core/aget cljs.core/* cljs.core/+ cljs.core//
                      #_cljs.core/let cljs.core/nil? cljs.core/aset clojure.core/gen-interface cljs.core/extend-type
                      cljs.core/implements? cljs.core/satisfies?])
      from-clj {#'cljs.core/defn #'clojure.core/defn
                #'cljs.core/defn- #'clojure.core/defn-
                #'cljs.core/declare #'clojure.core/declare
                #'cljs.core/defprotocol #'clojure.core/defprotocol
                #'clojure.core/deftype #'my-deftype
                #'cljs.core/deftype #'my-deftype}]
  (defn expand [a ns$ ls env [f & args :as o]]
    (if (symbol? f)
      (if (or (special? f) (ls f))
        o
        (if-some [mac (find-macro-var a f ns$)]
          (cond (= 'hyperfiddle.rcf/tests (symbol mac)) nil
                (= 'hyperfiddle.electric-de/defn (symbol mac)) `(def ~(first args))
                (blacklisted mac) o
                :else (apply (or (from-clj mac) mac) o env args))
          o))
      o)))

(defn ->def-info [ns$ sym] {::name (with-meta (symbol (str ns$) (str sym)) (meta sym)), ::meta (meta sym)})

(defn add-require [!a ns$ reqk from$ to$] (swap! !a assoc-in [::nses ns$ reqk from$] to$))

(defn add-refers [!a ns$ refk o req$]
  (reduce (fn [_ nx] (swap! !a assoc-in [::nses ns$ refk (or (get (:rename o) nx) nx)] (mksym req$ '/ nx)))
    nil (:refer o)))

(declare add-requireT analyze-nsT)

(defn noneT [s _f] (s nil) #())

(defn ?auto-alias-clojureT [!a ns$ reqk refk req$]
  (or (when-not (ns->resource req$)
        (let [cljs (str/replace-first (str req$) #"^clojure\." "cljs."), cljs$ (symbol cljs)]
          (when-not (= req$ cljs$)
            (when (ns->resource cljs$)
              (m/sp (m/? (add-requireT !a ns$ reqk refk [cljs$ :as req$]))  cljs$)))))
    noneT))

(defn add-requireT [!a ns$ reqk refk r]
  (let [r (if (or (symbol? r) (string? r)) [r] r)
        [req$ & o] r, o (apply hash-map o)]
    (if (= ns$ req$)
      noneT
      (m/sp
        (let [req$ (or (m/? (?auto-alias-clojureT !a ns$ reqk refk req$)) req$)]
          (add-require !a ns$ reqk req$ req$)
          (when (:as o) (add-require !a ns$ reqk (:as o) req$))
          (when (:refer o) (add-refers !a ns$ refk o req$))
          (m/? (m/join (fn [& _])
                 (analyze-nsT !a (->cljs-env ns$) req$)
                 (if (:refer-macros o)
                   (add-requireT !a ns$ reqk refk
                     (into [req$] cat (-> (select-keys o [:as]) (assoc :refer (:refer-macros o)))))
                   noneT))))))))

(defn -add-requiresT [!a ns$ rs reqk refk]
  (apply m/join (fn [& _]) (eduction (map #(add-requireT !a ns$ reqk refk %)) rs)))

(defn add-require-macrosT [!a ns$ rs] (-add-requiresT !a ns$ rs ::require-macros ::refer-macros))
(defn add-requiresT [!a ns$ rs] (-add-requiresT !a ns$ rs ::requires ::refers))
(defn add-refer-clojure [!a ns$ ov]
  (let [o (apply hash-map ov)]
    (when (:exclude o)
      (swap! !a assoc-in [::nses ns$ ::excludes] (set (:exclude o))))
    (when (:rename o)
      (swap! !a
        (fn [a]
          (-> a (update-in [::nses ns$ ::refers] merge
                  (reduce-kv (fn [m k v] (assoc m v (symbol "cljs.core" (name k)))) {} (:rename o)))
            (update-in [::nses ns$ ::excludes] into (keys (:rename o)))))))))
(defn use->require [args]
  (let [o (apply hash-map (next args))]
    (into [(first args)] cat (cond-> (select-keys o [:rename]) (:only o) (assoc :refer (:only o))))))

(comment
  (a-ns foo
    "docstring?"                                    ; DONE
    '{attr map?}                                    ; DONE
    (:refer-clojure :exclude [str])                 ; DONE
    (:refer-clojure :rename {str sstr})             ; DONE
    (:require x                                     ; DONE
              [x]                                   ; DONE
              [x :as xy]                            ; DONE
              [x :refer [y]]                        ; DONE
              [x :refer [y] :rename {y yy}]         ; DONE
              [x :include-macros true]              ; DONE
              [x :refer-macros [y]])                ; DONE
    (:require-macros x                              ; DONE
                     [x]                            ; DONE
                     [x :as xy]                     ; DONE
                     [x :refer [y]]                 ; DONE
                     [x :refer [y] :rename {y yy}]) ; DONE
    (:use x                                         ;
          [x]                                       ;
          [x :only [y]]                             ; DONE
          [x :only [y] :rename {y z}])              ; DONE
    (:use-macros x                                  ;
                 [x]                                ;
                 [x :only [y]]                      ; DONE
                 [x :only [y] :rename {y z}])       ; DONE
    )
  )
(defn skip-docstring [args] (cond-> args (string? (first args)) next))
(defn skip-attr-map [args] (cond-> args (map? (first args)) next))
(defn skip-inline-opts [args] (cond-> args (keyword? (first args)) (-> nnext recur)))
(defn add-ns-infoT [!a [_ns ns$ & args]]
  (let [args (-> args skip-docstring skip-attr-map)]
    (apply m/join (fn [& _])
      (eduction (map (fn [[typ & args]]
                       (case typ
                         (:require) (add-requiresT !a ns$ args)
                         (:require-macros) (add-require-macrosT !a ns$ args)
                         (:use) (add-requiresT !a ns$ (mapv use->require args))
                         (:use-macros) (add-require-macrosT !a ns$ (mapv use->require args))
                         (:refer-clojure) (m/sp (add-refer-clojure !a ns$ args))
                         #_else noneT)))
        args))))

(defn add-def [!a ns$ sym] (swap! !a assoc-in [::nses ns$ ::defs sym] (->def-info ns$ sym)))

(defn collect-defsT [!a ns$ env o]
  ;; (prn :defs (-> @!a ::nses (get ns$) ::defs keys sort))
  ;; (prn :collect-defs o)
  ((fn recT [ls !a o]
     (if (and (seq? o) (seq o))
       (case (first o)
         (defmacro clojure.core/defmacro cljs.core/defmacro) noneT

         (defprotocol clojure.core/defprotocol cljs.core/defprotocol)
         (let [[_ nm & args] o, fns (-> args skip-docstring skip-inline-opts)]
           (m/sp (run! #(add-def !a ns$ %) (cons nm (eduction (map first fns))))))

         (def) (m/sp (add-def !a ns$ (second o)))

         (deftype clojure.core/deftype cljs.core/deftype)
         (let [[_ nm] o] (m/sp (add-def !a ns$ nm) (add-def !a ns$ (mksym '-> nm))))

         (ns) (add-ns-infoT !a o)

         (fn*) (let [body (if (symbol? (second o)) (nnext o) (next o))
                     arities (if (vector? (first body)) (list body) body)]
                 (apply m/join (fn [& _])
                   (eduction (map (fn [[bs & body]] (recT (into ls bs) !a (cons 'do body)))) arities)))

         (let*) (let [[_ bs & body] o
                      [Ts ls] (transduce (partition-all 2)
                                (completing (fn [[Ts ls] [k v]] [(conj Ts (recT ls !a v)) (conj ls k)]))
                                [[] ls] bs)]
                  (apply m/join (fn [& _]) (conj Ts (recT ls !a (cons 'do body)))))

         #_else (let [o2 (expand @!a ns$ ls env o)]
                  (if (identical? o o2)
                    (apply m/join (fn [& _]) (eduction (map #(recT ls !a %)) o))
                    (recur ls !a o2))))
       noneT))
   #{} !a o))

(defn keep-if [v pred] (when (pred v) v))
(defn macro-var? [vr] (and (instance? clojure.lang.Var vr) (.isMacro ^clojure.lang.Var vr)))

;;;;;;;;;;;;;;;;;;
;;; PUBLIC API ;;;
;;;;;;;;;;;;;;;;;;

(defn analyze-nsT [!a env ns$]
  (if-some [rs (some-> ns$ ns->resource)]
    (loop [a @!a]
      (or (-> a ::ns-tasks (get ns$))
        (let [T (->> (m/ap (let [o (m/?> (resource-forms> rs))]
                             (m/? (collect-defsT !a ns$ env o))))
                  (m/reduce (fn [_ _]) nil))
              T (m/memo (m/via m/blk (m/? T)))]
          (if (compare-and-set! !a a (assoc-in a [::ns-tasks ns$] T))
            T
            (recur @!a)))))
    noneT))

(defn purge-ns [!a ns$] (swap! !a (fn [a] (-> a (update ::ns-tasks dissoc ns$) (update ::nses dissoc ns$)))) nil)

(defn find-var [a sym ns$]
  (let [nsa (-> a ::nses (get ns$))]
    (if (simple-symbol? sym)
      (or (-> nsa ::defs (get sym))
        (when-not (get (::excludes nsa) sym)
          (-> a ::nses (get 'cljs.core) ::defs (get sym)))
        (when-some [renamed (get (::refers nsa) sym)]
          (-> a ::nses (get (symbol (namespace renamed))) ::defs (get (symbol (name renamed))))))
      (or (when-some [sym-ns$ (-> nsa ::requires (get (symbol (namespace sym))))]
            (find-var a (symbol (name sym)) sym-ns$))
        (when (= "clojure.core" (namespace sym))
          (-> a ::nses (get 'cljs.core) ::defs (get (-> sym name symbol))))
        (-> a ::nses (get (-> sym namespace symbol)) ::defs (get (-> sym name symbol)))))))

;; TODO clojure.core -> cljs.core, clojure.repl -> cljs.repl
(defn find-macro-var [a sym ns$]
  (when-not (find-var a sym ns$)
    (-> (cond
          (simple-symbol? sym)
          (or (do (safe-require ns$)  (some-> (find-ns ns$) (find-ns-var sym)))
            (when-some [ref (-> a ::nses (get ns$) ::refers (get sym))]  (requiring-resolve ref))
            (when-some [ref (-> a ::nses (get ns$) ::refer-macros (get sym))]  (requiring-resolve ref))
            (when-not (get (-> a ::nses (get ns$) ::excludes) sym)  (find-ns-var (find-ns 'clojure.core) sym)))

          (#{"cljs.core" "clojure.core"} (namespace sym))
          (requiring-resolve sym)

          :else
          (let [sym-ns$ (-> sym namespace symbol), sym-base$ (-> sym name symbol)]
            (or (when-some [sym-ns$ (-> a ::nses (get ns$) ::requires (get sym-ns$))]
                (safe-require sym-ns$)
                (some-> (find-ns sym-ns$) (find-ns-var sym-base$)))
              (when-some [sym-ns$ (-> a ::nses (get ns$) ::require-macros (get sym-ns$))]
                (safe-require sym-ns$)
                (some-> (find-ns sym-ns$) (find-ns-var sym-base$)))
              (some-> (find-ns sym-ns$) (find-ns-var sym-base$)))))
      (keep-if macro-var?))))

(defn ->!a [] (let [!a (atom {})] (m/? (analyze-nsT !a (->cljs-env 'cljs.core) 'cljs.core)) !a))
