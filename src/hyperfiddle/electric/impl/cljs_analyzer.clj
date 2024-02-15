(ns hyperfiddle.electric.impl.cljs-analyzer
  (:refer-clojure :exclude [find-var])
  (:require [edamame.core :as ed]
            [clojure.core :as cc]
            [clojure.tools.reader.reader-types :as rt]
            [clojure.java.io :as io]
            [cljs.tagged-literals]
            [missionary.core :as m]
            [contrib.debug]
            [cljs.core]                 ; for cljs macroexpansion
            [cljs.env]
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

(let [parse-opts (ed/normalize-opts {:all true
                                     :row-key :line
                                     :col-key :column
                                     :end-location false
                                     :readers cljs.tagged-literals/*cljs-data-readers*
                                     :auto-resolve name
                                     :eof ::done})]
  (defn ns-forms> [ns$]
    (->> (m/ap
           (let [rdr (m/?> (m/observe (fn [!]
                                        (let [rdr (rt/source-logging-push-back-reader (io/reader (ns->resource ns$)))]
                                          (! rdr) #(.close ^java.io.Reader rdr)))))]
             (m/? (m/?> (m/seed (repeat (m/via m/blk (ed/parse-next rdr parse-opts))))))))
      (m/eduction (take-while (complement #{::done}))))))

(comment
  (m/? (m/reduce conj (ns-forms> 'hyperfiddle.electric.impl.cljs-file-to-analyze)))
  )

(declare analyze-ns)
(defn ->a [ns$] (assoc (analyze-ns {} 'cljs.core) ::current-ns ns$))

(defn safe-require [sym]
  ;; we might be expanding clj code before the ns got loaded (during cljs compilation)
  ;; to correctly lookup vars the ns needs to be loaded
  ;; since shadow-cljs compiles in parallel we need to serialize the requires
  (when-not (get (loaded-libs) sym)
    (try (#'clojure.core/serialized-require sym) ; try bc it can be cljs file
         (catch java.io.FileNotFoundException _))))

(defn find-ns-var [^clojure.lang.Namespace nso sym] (.findInternedVar nso sym))
(declare find-var)

(defn runtime-var? [f a] (or (find-var f a) (find-var f (assoc a ::current-ns 'cljs.core))))

(defn mksym [& xs] (symbol (apply str (mapv #((if (or (keyword? %) (symbol? %)) name str) %) xs))))

(defmacro my-deftype [nm & _] `(do (def ~nm) (def ~(mksym '-> nm))))
(let [blacklisted (into #{} (map cc/find-var) '[cljs.core/exists? cljs.core/str cljs.core/aget cljs.core/* cljs.core/+ cljs.core// cljs.core/let cljs.core/nil? cljs.core/aset clojure.core/gen-interface cljs.core/extend-type])
      from-clj {#'cljs.core/defn #'clojure.core/defn
                #'cljs.core/defn- #'clojure.core/defn-
                #'cljs.core/declare #'clojure.core/declare
                #'cljs.core/defprotocol #'clojure.core/defprotocol
                #'cljs.core/deftype #'my-deftype}]
  (defn ?expand [ns$ name$ o]
    (safe-require ns$)
    (let [vr (some-> (find-ns ns$) (find-ns-var name$))]
      (if (and vr (.isMacro ^clojure.lang.Var vr))
        (if-some [clj (from-clj vr)]
          (apply clj o {} (next o))
          (if (blacklisted vr)
            o
            (apply vr o {} (next o))))
        o))))

(def special? '#{if def fn* do let* loop* letfn* throw try catch finally
                 recur new set! ns deftype* defrecord* . js* & quote case* var ns*})

(defn qualified->parts [qs a]
  (let [ns$ (-> qs namespace symbol)]
    [(or (-> a ::nses (get (::current-ns a)) ::requires (get ns$))
       (-> a ::nses (get (::current-ns a)) ::require-macros (get ns$))
       ns$) (-> qs name symbol)]))

(defn simple->parts [s a]
  (let [s (or (-> a ::nses (get (::current-ns a)) ::refers (get s))
            (-> a ::nses (get (::current-ns a)) ::refer-macros (get s))
            (mksym (::current-ns a) '/ s))]
    [(-> s namespace symbol) (-> s name symbol)]))

(defn expand [[f :as o] a]
  ;; TODO locals, refers
  (if (symbol? f)
    (cond
      (special? f) o
      (qualified-symbol? f) (let [[ns$ name$] (qualified->parts f a)]
                              (?expand ns$ name$ o))
      (runtime-var? f a) o
      :else (let [[ns$ name$] (simple->parts f a)
                  o2 (?expand ns$ name$ o)]
              (if (identical? o o2)
                (?expand 'cljs.core f o)
                o2)))
    o))

(defn ->def-info [[_def sym _v :as o]] {::name sym, ::meta (merge (meta sym) (meta o))})

(defn add-require [a ns$ reqk from$ to$] (assoc-in a [::nses ns$ reqk from$] to$))

(defn add-refers [a ns$ refk o req$]
  (reduce (fn [a nx] (assoc-in a [::nses ns$ refk (or (get (:rename o) nx) nx)] (mksym req$ '/ nx)))
    a (:refer o)))

(defn -add-require [a ns$ reqk refk r]
  (if (or (symbol? r) (string? r))
    (add-require a ns$ reqk r r)
    (let [[req$ & o] r, a (add-require a ns$ reqk req$ req$), o (apply hash-map o)]
      (cond-> a
        (:as o) (add-require ns$ reqk (:as o) req$)
        (:refer o) (add-refers ns$ refk o req$)
        (:refer-macros o) (recur ns$ reqk refk (into [req$] cat (-> (select-keys o [:as]) (assoc :refer (:refer-macros o)))))))))

(defn -add-requires [a ns$ rs reqk refk] (reduce #(-add-require % ns$ reqk refk %2) a rs))

(defn add-require-macros [a ns$ rs] (-add-requires a ns$ rs ::require-macros ::refer-macros))
(defn add-requires [a ns$ rs] (-add-requires a ns$ rs ::requires ::refers))
(defn reverse-map [m] (persistent! (reduce-kv (fn [m k v] (assoc! m v k)) (transient {}) m)))
(defn add-refer-clojure [a ns$ ov]
  (let [o (apply hash-map ov)]
    (cond-> a
      (:exclude o) (assoc-in [::nses ns$ ::excludes] (set (:exclude o)))
      (:rename o) (assoc-in [::nses ns$ ::renames] (reverse-map (:rename o))))))
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
(defn add-ns-info [a [_ns ns$ & args]]
  (let [args (-> args skip-docstring skip-attr-map)]
    (reduce (fn [a [typ & args]]
              (case typ
                (:require) (add-requires a ns$ args)
                (:require-macros) (add-require-macros a ns$ args)
                (:use) (add-requires a ns$ (mapv use->require args))
                (:use-macros) (add-require-macros a ns$ (mapv use->require args))
                (:refer-clojure) (add-refer-clojure a ns$ args)
                #_else a)) a args )))

(defn collect-defs [a o]
  (if (and (seq? o) (seq o))
    (case (first o)
      (def) (assoc-in a [::nses (::current-ns a) ::defs (second o)] (->def-info o))
      (ns) (add-ns-info a o)
      ;; (fn* foo [x] x) (fn* foo ([x] x) ([x y] x)) (fn* [x] x) (fn* ([x] x) ([x y] x))
      (fn*) (let [body (if (symbol? (second o)) (nnext o) (next o))
                  arities (if (vector? (first body)) (list body) body)]
              (transduce (map #(expand (next %) a)) (completing collect-defs) a arities))
      #_else (let [o2 (expand o a)]
               (if (identical? o o2)
                 (reduce collect-defs a (expand o a))
                 (collect-defs a o2))))
    a))

;;;;;;;;;;;;;;;;;;
;;; PUBLIC API ;;;
;;;;;;;;;;;;;;;;;;

(defn analyze-ns
  ([ns$] (analyze-ns (->a ns$) ns$))
  ([a ns$] (->> (ns-forms> ns$) (m/reduce collect-defs (assoc a ::current-ns ns$)) m/?)))

(defn find-var [sym a]
  (let [nsa (-> a ::nses (get (::current-ns a))), cljs-defs (-> a ::nses (get 'cljs.core) ::defs)]
    (or (-> nsa ::defs (get sym))
      (when-not (get (-> nsa ::excludes) sym) (get cljs-defs sym))
      (when-some [renamed (get (-> nsa ::renames) sym)] (get cljs-defs renamed)))))







;; probably trash


#_(defn ?expand [ns$ name$ o]
    (safe-require ns$)
    (let [vr (some-> (find-ns ns$) (find-ns-var name$))]
      (if (and vr (.isMacro ^clojure.lang.Var vr))
        (apply vr o {} (next o))
        o)))

#_(defn ?expand [ns$ name$ o]
  (safe-require ns$)
  (let [vr (some-> (find-ns ns$) (find-ns-var name$))]
    (if (and vr (.isMacro ^clojure.lang.Var vr))
      (apply vr o {} (next o))
      o)))

#_(defn expand [[f :as o] a]
  ;; TODO locals, refers
  (cond
    (qualified-symbol? f) (if (and (= 'cljs.core (::current-ns a)) (= "cljs.core" (namespace f)))
                            o
                            (let [sym (unalias f a), ns$ (-> sym namespace symbol), name$ (-> sym name symbol)]
                              (?expand ns$ name$ o)))
    (runtime-var? f a) o
    (= 'cljs.core (::current-ns a)) o
    :else (let [o2 (?expand (::current-ns a) f o)]
            (if (identical? o o2)
              (?expand 'cljs.core f o)
              o))))
