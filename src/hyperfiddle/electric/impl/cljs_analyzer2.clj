(ns hyperfiddle.electric.impl.cljs-analyzer2
  (:refer-clojure :exclude [find-var])
  (:require [cljs.analyzer]
            [cljs.core] ; for cljs macroexpansion
            [cljs.env]
            [cljs.repl]
            [cljs.tagged-literals]
            [clojure.core :as cc]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.reader.reader-types :as rt]
            [contrib.assert :as ca]
            [edamame.core :as ed]                 ; for cljs macroexpansion
            [clojure.walk :as walk]))

(defn ns->basename [ns$] (-> ns$ name (.replace \- \_) (.replace \. \/)))

(defn ns->resource [ns$]
  (let [base (ns->basename ns$)]
    (or (io/resource (str base ".cljs"))
      (io/resource (str base ".cljc")))))

(defn eval-edamame-read-eval
  "Edamame, an edn parser, doesn't fully implement read-eval (aka `#=`).
  Edamame expands `#=(foo bar)` to `(edamame.core/read-eval (foo bar))` and
  leave the actual eval action's responsibility to the user -
  `edamame.core/read-eval` does not resolve to any var.

  The current function takes an edamame-parsed form, walk it and eval parsed #=
  forms. Edamame must be invoked with `{:read-eval true, :quote true}` options
  to produce a form we can properly eval.

  Doesn't preserve read-time metadata. We don't care about metadata in this
  specific case, because we only use this to read ns forms and we are not aware
  of any use case for metadata in ns forms applying to Electric"
  [form]
  (walk/prewalk
    (fn [form] (if (and (seq? form) (= 'edamame.core/read-eval (first form))) (eval (second form)) form))
    form))

(let [parse-opts
      (ed/normalize-opts {:all true, :row-key :line, :col-key :column, :end-location false
                          :readers cljs.tagged-literals/*cljs-data-readers* :auto-resolve name
                          :features #{:cljs}, :read-cond :allow, :read-eval true, :quote true, :eof ::done})]
  (defn resource-forms [rs]
    (with-open [rdr (rt/source-logging-push-back-reader (io/reader rs))]
      (loop [v []]
        (let [nx (eval-edamame-read-eval (ed/parse-next rdr parse-opts))]
          (if (= nx ::done) v (recur (conj v nx))))))))

(defn safe-require [sym]
  ;; we might be expanding clj code before the ns got loaded (during cljs compilation)
  ;; to correctly lookup vars the ns needs to be loaded
  ;; since shadow-cljs compiles in parallel we need to serialize the requires
  (when-not (get (loaded-libs) sym)
    (try (#'clojure.core/serialized-require sym) ; try bc it can be cljs file
         (catch java.io.FileNotFoundException _)
         (catch Throwable t ; HACK temporary fix. Electric tries to load `.cljc` files assuming it contains clojure code, but fails for cljc files only targeting multip cljs targets (e.g. nextjournal.clojure-mode.util targets :squint + :cljs)
                            ; Ignoring the failed require seems harmless. Log to keep an eye on it and detect more failing cases.
           (print `safe-require "Electric failed to load ns for" sym ":" (ex-message t))))))

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
      short-circuit-def '#{clojure.core/defn, cljs.core/defn, clojure.core/defn-, cljs.core/defn-, cljs.spec.alpha/def}
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
                  (= 'hyperfiddle.electric3/defn sym) `(def ~(first args)) ; circular, don't go deeper
                  (short-circuit-def sym) `(def ~(first args))
                  (declare? sym) `(do ~@(mapv #(list 'def %) args))
                  (deftype? sym) (let [[nm] args] `(declare ~nm ~(mksym '-> nm)))
                  (defrecord? sym) (let [[nm] args] `(declare ~nm ~(mksym '-> nm) ~(mksym 'map-> nm)))
                  (defmacro? sym) nil
                  (defprotocol? sym) (let [[_ nm & args] o, fns (-> args skip-docstring skip-inline-opts)]
                                       `(declare ~nm ~@(mapv first fns)))
                  (blacklisted sym) o   ; reading compiler atom *during macroexpansion*
                  :else (try (apply mac o env args)
                             (catch Throwable e (prn :cannot-expand (::ns-stack env) (cons mac args)) (throw e)))))
          o))
      o)))

(defn add-require [!a ns$ reqk from$ to$] (swap! !a assoc-in [::nses ns$ reqk from$] to$))

(defn add-refers [!a ns$ refk o req$]
  (reduce (fn [_ nx] (swap! !a assoc-in [::nses ns$ refk (or (get (:rename o) nx) nx)] (mksym req$ '/ nx)))
    nil (:refer o)))

(declare add-requireT analyze-nsT)

(defn ?auto-alias-clojureT [!a ns$ env reqk refk req$]
  (when-not (ns->resource req$)
    (let [cljs (str/replace-first (str req$) #"^clojure\." "cljs."), cljs$ (symbol cljs)]
      (when-not (= req$ cljs$)
        (when (ns->resource cljs$)
          (add-requireT !a ns$ env reqk refk [cljs$ :as req$])
          cljs$)))))

(defn add-requireT [!a ns$ env reqk refk r]
  (let [r (if (or (symbol? r) (string? r)) [r] r)
        [req$ & o] r, o (apply hash-map o)]
    (when (not= ns$ req$)
      (let [req$ (or (?auto-alias-clojureT !a ns$ env reqk refk req$) req$)]
        (add-require !a ns$ reqk req$ req$)
        (when (:as o) (add-require !a ns$ reqk (:as o) req$))
        (when (:refer o) (add-refers !a ns$ refk o req$))
        (analyze-nsT !a (assoc env :ns {:name ns$}) #_(->cljs-env ns$) req$)
        (when (:refer-macros o)
          (add-requireT !a ns$ env reqk refk
            (into [req$] cat (-> (select-keys o [:as]) (assoc :refer (:refer-macros o))))))))))

(defn -add-requiresT [!a ns$ env rs reqk refk]
  (run! #(add-requireT !a ns$ env reqk refk %) rs))

(defn add-require-macrosT [!a ns$ env rs] (-add-requiresT !a ns$ env rs ::require-macros ::refer-macros))
(defn add-requiresT [!a ns$ env rs] (-add-requiresT !a ns$ env rs ::requires ::refers))
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

(letfn [(simple [v] (-> v str (str/split #"\.") peek symbol))]
  (defn add-1-import [ns$ a v]
    (let [add (fn add [a v] (update-in a [::nses ns$ ::imports] (fnil conj #{}) v (simple v)))]
      (if (symbol? v)
        (add a v)                       ; java.util.X
        (if (next v)                    ; [java.util X Y]
          (reduce (fn [a nx] (add a (mksym (first v) "." nx))) a (next v))
          (add a (first v)))))))        ; [java.util.X]

(defn add-import [!a ns$ args]
  (swap! !a (fn [a] (reduce (partial add-1-import ns$) a args))))

(defn add-ns-infoT [!a env [_ns ns$ & args]]
  (let [args (-> args skip-docstring skip-attr-map)]
    (run! (fn [[typ & args]]
            (case typ
              (:require) (add-requiresT !a ns$ env args)
              (:require-macros) (add-require-macrosT !a ns$ env args)
              (:use) (add-requiresT !a ns$ env (mapv use->require args))
              (:use-macros) (add-require-macrosT !a ns$ env (mapv use->require args))
              (:refer-clojure) (add-refer-clojure !a ns$ args)
              (:import) (add-import !a ns$ args)
              #_else nil))
      args)))

(defn ->def-info [ns$ sym] {::name (with-meta (symbol (str ns$) (str sym)) (meta sym)), ::meta (meta sym)})

(defn add-def [!a ns$ sym] (swap! !a assoc-in [::nses ns$ ::defs sym] (->def-info ns$ sym)))

(defn collect-defs [!a ns$ env o]
  ((fn rec [ls !a o]
     (when (and (seq? o) (seq o))
       (case (first o)
         (def) (add-def !a ns$ (second o))
         (ns) (add-ns-infoT !a env o)
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
  (ca/is ns$ (complement #{'hyperfiddle.electric}) "cannot analyze old electric code")
  (when-some [rs (some-> ns$ ns->resource)]
    (let [env (update env ::ns-stack (fnil conj []) ns$)]
      (try (loop [a @!a]
             (or (-> a ::ns-tasks (get ns$))
               (if (compare-and-set! !a a (assoc-in a [::ns-tasks ns$] true))
                 (->> (resource-forms rs) (reduce #(collect-defs !a ns$ env %2) nil))
                 (recur @!a))))
           (catch Throwable e
             (prn :failed-to-analyze (::ns-stack env))
             (throw e))))))

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

(defn ns-qualify [a sym ns$]
  (when-some [qual-ns (keep-if (-> a ::nses (get ns$) ::requires (get (-> sym namespace symbol))) symbol?)]
    (symbol (str qual-ns) (name sym))))

(def implicit-nses '#{goog goog.object goog.string goog.array Math String})

(defn imported? [a sym ns$]
  (let [imports (into implicit-nses (-> a ::nses (get ns$) ::imports))
        dot-access (-> sym str (str/replace #"\.[^.]+$" "") symbol)]
    (or (get imports dot-access)
      (and (qualified-symbol? sym) (get imports (-> sym namespace symbol))))))

(defn referred? [a sym ns$] (-> a ::nses (get ns$) ::refers (get sym)))
