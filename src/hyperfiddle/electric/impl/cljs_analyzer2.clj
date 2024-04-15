(ns hyperfiddle.electric.impl.cljs-analyzer2
  (:refer-clojure :exclude [find-var])
  (:require [edamame.core :as ed]
            [clojure.core :as cc]
            [clojure.string :as str]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.java.io :as io]
            [cljs.tagged-literals]
            [contrib.debug]
            [cljs.core]                 ; for cljs macroexpansion
            [cljs.env]
            [cljs.analyzer]
            [cljs.repl]))               ; for cljs macroexpansion

(defn ns->basename [ns$] (-> ns$ name (.replace \- \_) (.replace \. \/)))

(defn ns->resource [ns$]
  (let [base (ns->basename ns$)]
    (or (io/resource (str base ".cljs"))
      (io/resource (str base ".cljc")))))

(let [parse-opts
      (ed/normalize-opts {:all true, :row-key :line, :col-key :column, :end-location false
                          :readers cljs.tagged-literals/*cljs-data-readers* :auto-resolve name
                          :features #{:cljs}, :read-cond :allow, :eof ::done})]
  (defn resource-forms [rs]
    (with-open [rdr (rt/source-logging-push-back-reader (io/reader rs))]
      (loop [v []]
        (let [nx (ed/parse-next rdr parse-opts)]
          (if (= nx ::done) v (recur (conj v nx))))))))

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

(defn skip-docstring [args] (cond-> args (string? (first args)) next))
(defn skip-attr-map [args] (cond-> args (map? (first args)) next))
(defn skip-inline-opts [args] (cond-> args (keyword? (first args)) (-> nnext recur)))

(let [blacklisted '#{cljs.core/exists? cljs.core/str cljs.core/extend-type}
      short-circuit-def '#{clojure.core/defn, cljs.core/defn, clojure.core/defn-, cljs.core/defn-}
      declare? '#{clojure.core/declare cljs.core/declare}
      deftype? '#{clojure.core/deftype cljs.core/deftype}
      defrecord? '#{clojure.core/defrecord cljs.core/defrecord}
      defmacro? '#{clojure.core/defmacro cljs.core/defmacro}
      defprotocol? '#{clojure.core/defprotocol cljs.core/defprotocol}]
  (defn expand [a ns$ ls env [f & args :as o]]
    (if (symbol? f)
      (if (or (special? f) (ls f))
        o
        (if-some [mac (find-macro-var a f ns$)]
          (let [sym (symbol mac)]
            (cond (= 'hyperfiddle.rcf/tests sym) nil ; circular, we can skip rcf tests
                  (= 'hyperfiddle.electric-de/defn sym) `(def ~(first args)) ; circular, don't go deeper
                  (short-circuit-def sym) `(def ~(first args))
                  (declare? sym) `(do ~@(mapv #(list 'def %) args))
                  (deftype? sym) (let [[nm] args] `(declare ~nm ~(mksym '-> nm)))
                  (defrecord? sym) (let [[nm] args] `(declare ~nm ~(mksym '-> nm) ~(mksym 'map-> nm)))
                  (defmacro? sym) nil
                  (defprotocol? sym) (let [[_ nm & args] o, fns (-> args skip-docstring skip-inline-opts)]
                                       `(declare ~nm ~@(mapv first fns)))
                  (blacklisted sym) o   ; reading compiler atom *during macroexpansion*
                  :else (apply mac o env args)))
          o))
      o)))

(defn add-require [!a ns$ reqk from$ to$] (swap! !a assoc-in [::nses ns$ reqk from$] to$))

(defn add-refers [!a ns$ refk o req$]
  (reduce (fn [_ nx] (swap! !a assoc-in [::nses ns$ refk (or (get (:rename o) nx) nx)] (mksym req$ '/ nx)))
    nil (:refer o)))

(declare add-requireT analyze-nsT)

(defn ?auto-alias-clojureT [!a ns$ reqk refk req$]
  (when-not (ns->resource req$)
    (let [cljs (str/replace-first (str req$) #"^clojure\." "cljs."), cljs$ (symbol cljs)]
      (when-not (= req$ cljs$)
        (when (ns->resource cljs$)
          (add-requireT !a ns$ reqk refk [cljs$ :as req$])
          cljs$)))))

(defn add-requireT [!a ns$ reqk refk r]
  (let [r (if (or (symbol? r) (string? r)) [r] r)
        [req$ & o] r, o (apply hash-map o)]
    (when (not= ns$ req$)
      (let [req$ (or (?auto-alias-clojureT !a ns$ reqk refk req$) req$)]
        (add-require !a ns$ reqk req$ req$)
        (when (:as o) (add-require !a ns$ reqk (:as o) req$))
        (when (:refer o) (add-refers !a ns$ refk o req$))
        (analyze-nsT !a (->cljs-env ns$) req$)
        (when (:refer-macros o)
          (add-requireT !a ns$ reqk refk
            (into [req$] cat (-> (select-keys o [:as]) (assoc :refer (:refer-macros o))))))))))

(defn -add-requiresT [!a ns$ rs reqk refk]
  (run! #(add-requireT !a ns$ reqk refk %) rs))

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

(defn add-ns-infoT [!a [_ns ns$ & args]]
  (let [args (-> args skip-docstring skip-attr-map)]
    (run! (fn [[typ & args]]
            (case typ
              (:require) (add-requiresT !a ns$ args)
              (:require-macros) (add-require-macrosT !a ns$ args)
              (:use) (add-requiresT !a ns$ (mapv use->require args))
              (:use-macros) (add-require-macrosT !a ns$ (mapv use->require args))
              (:refer-clojure) (add-refer-clojure !a ns$ args)
              #_else nil))
      args)))

(defn ->def-info [ns$ sym] {::name (with-meta (symbol (str ns$) (str sym)) (meta sym)), ::meta (meta sym)})

(defn add-def [!a ns$ sym] (swap! !a assoc-in [::nses ns$ ::defs sym] (->def-info ns$ sym)))

(defn collect-defs [!a ns$ env o]
  ((fn rec [ls !a o]
     (when (and (seq? o) (seq o))
       (case (first o)
         (def) (add-def !a ns$ (second o))
         (ns) (add-ns-infoT !a o)
         (fn*) nil
         (let*) (let [[_ bs & body] o
                      ls (transduce (partition-all 2) (completing (fn [ls [k v]] (rec ls !a v) (conj ls k))) ls bs)]
                  (rec ls !a (cons 'do body)))
         #_else (let [o2 (expand @!a ns$ ls env o)]
                  (if (identical? o o2)
                    (run! #(rec ls !a %) o)
                    (recur ls !a o2))))))
   #{} !a o))

(defn keep-if [v pred] (when (pred v) v))
(defn macro-var? [vr] (and (instance? clojure.lang.Var vr) (.isMacro ^clojure.lang.Var vr)))

(defn safe-requiring-resolve [sym] (try (requiring-resolve sym) (catch java.io.FileNotFoundException _)))

;;;;;;;;;;;;;;;;;;
;;; PUBLIC API ;;;
;;;;;;;;;;;;;;;;;;

(def !nss (atom {}))

(defn analyze-nsT [!a env ns$]
  (when-some [rs (some-> ns$ ns->resource)]
    (loop [a @!a]
      (or (-> a ::ns-tasks (get ns$))
        (if (compare-and-set! !a a (assoc-in a [::ns-tasks ns$] true))
          (->> (resource-forms rs) (reduce #(collect-defs !a ns$ env %2) nil))
          (recur @!a))))))

(defn purge-ns [a ns$] (-> a (update ::ns-tasks dissoc ns$) (update ::nses dissoc ns$)))

(defn find-var [a sym ns$]
  (let [nsa (-> a ::nses (get ns$))]
    (if (simple-symbol? sym)
      (or (-> nsa ::defs (get sym))
        (when-not (get (::excludes nsa) sym)
          (-> a ::nses (get 'cljs.core) ::defs (get sym)))
        (when-some [renamed (get (::refers nsa) sym)]
          (-> a ::nses (get (symbol (namespace renamed))) ::defs (get (symbol (name renamed))))))
      (or (-> a ::nses (get (-> sym namespace symbol)) ::defs (get (-> sym name symbol)))
        (when-some [sym-ns$ (-> nsa ::requires (get (symbol (namespace sym))))]
            (find-var a (symbol (name sym)) sym-ns$))
        (when (= "clojure.core" (namespace sym))
          (-> a ::nses (get 'cljs.core) ::defs (get (-> sym name symbol))))))))

;; cljs analyzer has extra, clojure.core -> cljs.core, clojure.repl -> cljs.repl, do we need it?
(defn find-macro-var [a sym ns$]
  (when-not (find-var a sym ns$)
    (-> (cond
          (simple-symbol? sym)
          (or (do (safe-require ns$)  (some-> (find-ns ns$) (find-ns-var sym)))
            (when-some [ref (-> a ::nses (get ns$) ::refers (get sym))]  (safe-requiring-resolve ref))
            (when-some [ref (-> a ::nses (get ns$) ::refer-macros (get sym))]  (safe-requiring-resolve ref))
            (when-not (get (-> a ::nses (get ns$) ::excludes) sym)  (find-ns-var (find-ns 'clojure.core) sym)))

          (#{"cljs.core" "clojure.core"} (namespace sym))
          (safe-requiring-resolve sym)

          :else
          (let [sym-ns$ (-> sym namespace symbol), sym-base$ (-> sym name symbol)]
            (or (when-some [sym-ns$ (-> a ::nses (get ns$) ::requires (get sym-ns$))]
                  (when (symbol? sym-ns$)
                    (safe-require sym-ns$)
                    (some-> (find-ns sym-ns$) (find-ns-var sym-base$))))
              (when-some [sym-ns$ (-> a ::nses (get ns$) ::require-macros (get sym-ns$))]
                (when (symbol? sym-ns$)
                  (safe-require sym-ns$)
                  (some-> (find-ns sym-ns$) (find-ns-var sym-base$))))
              (some-> (find-ns sym-ns$) (find-ns-var sym-base$)))))
      (keep-if macro-var?))))

(defn ->!a [] (let [!a (atom {})] (analyze-nsT !a (->cljs-env 'cljs.core) 'cljs.core) !a))

(defn- referred-from-js-require? [a ns$ ref] (-> a ::nses (get ns$) ::requires (get (namespace ref))))

(defn js-call? [a sym ns$]
  (if (qualified-symbol? sym)
    (or (= "js" (namespace sym))
      (string? (-> a ::nses (get ns$) ::requires (get (-> sym namespace symbol)))))
    (when-some [ref (-> a ::nses (get ns$) ::refers (get sym))]
      (referred-from-js-require? a ns$ ref))))
