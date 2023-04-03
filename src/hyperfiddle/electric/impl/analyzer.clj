(ns hyperfiddle.electric.impl.analyzer
  "Utilities to analyze and transform Clojure/Script code (not Electric code).
  Use case: support `clojure.core/fn` in an Electric program.
  Long term goal is to build a unified Clojure/Script code walker and get rid of tools.analyzer deps."
  (:require [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.tools.analyzer.ast :as clj-ast]
            [clojure.tools.analyzer.jvm :as clj]
            [cljs.analyzer.api :as cljs]
            [cljs.analyzer :as cljs-ana]
            [cljs.analyzer.passes :as cljs-ast]
            [clojure.tools.logging :as log]))

(defn var-name [ast]
  (if-let [var (:var ast)]
    (.toSymbol var)
    (or (:name ast)
        (:form ast))))

(defn walk-clj "Prewalk a clj ast" [ast f] (clj-ast/prewalk ast f))
(defn walk-cljs "Prewalk a cljs ast" [ast f] (cljs-ast/walk ast [(fn [env ast opts] (f ast))]))

(defn macroexpand-1-clj
  ;; like clj/macroexpand-1 except:
  ;; - doesn't inline - not needed
  ;; - drop tags - not needed and can cause issues in emitted code (Class object in cljs compiler)
  ;; - *env* is not ensured - expensive to compute and not needed in this context
  "If form represents a macro form, returns its expansion, else returns form."
  ([form] (macroexpand-1-clj form (clj/empty-env)))
  ([form env]
   (cond
     (seq? form)
     (let [[op & args] form]
       (if (clj/specials op)
         form
         (let [v      (clojure.tools.analyzer.utils/resolve-sym op env)
               m      (meta v)
               local? (-> env :locals (get op))
               macro? (and (not local?) (:macro m))] ; locals shadow macros
           (cond
             macro? (let [res (apply v form (:locals env) (rest form))] ; (m &form &env & args)
                      (when-not (clj/ns-safe-macro v)
                        (clj/update-ns-map!))
                      (if (clojure.tools.analyzer.utils/obj? res)
                        (vary-meta res merge (meta form))
                        res))
             :else  (clj/desugar-host-expr form env)))))
     (symbol? form) (clj/desugar-symbol form env)
     :else          form)))

(defn analyze-clj "Analyze a clj form to ast without any passes." [env form]
  (binding [clj/run-passes identity]
    (clj/analyze form env {:bindings {#'clojure.tools.analyzer/macroexpand-1 macroexpand-1-clj}})))

(defn analyze-cljs "Analyze a cljs form to ast without any passes." [env form]
  (binding [cljs-ana/*cljs-ns* (:name (:ns env))
            cljs-ana/*passes*  []]
    (cljs/analyze env form #_#_nil {:warning-handlers [(fn [type env info] (prn type))]})))

(defn specialize-clj-ast-op [ast]
  (update ast :op (fn [op] (case op
                             :const ::const
                             op))))
(defn emit-clj [ast]
  (emit-form/emit-form (walk-clj ast specialize-clj-ast-op)))

(defmethod emit-form/-emit-form ::const
  [{:keys [type val] :as ast} opts]
  (if (= type :class)
    (symbol (.getName ^Class val))
    (emit-form/-emit-form (assoc ast :op :const) opts)))


(declare emit-cljs)
(defn emit-cljs-method [{:keys [variadic? params body]}]
  (list (if variadic?
          (-> (into [] (map :name) (pop params))
            (conj '& (-> params peek :name)))
          (into [] (map :name) params)) (emit-cljs body)))

;; Adapted from leonoel/injure
(defn emit-cljs "Emit cljs code from a cljs.analyzer AST."
  [ast]
  (case (:op ast)
    :let
    (list 'let (into [] (mapcat
                          (fn [{:keys [name init]}]
                            [name (emit-cljs init)]))
                 (:bindings ast))
      (emit-cljs (:body ast)))

    :loop
    (list 'loop (into [] (mapcat
                           (fn [{:keys [name init]}]
                             [name (emit-cljs init)]))
                  (:bindings ast))
      (emit-cljs (:body ast)))

    :recur
    (cons 'recur (map emit-cljs (:exprs ast)))

    :invoke
    (map emit-cljs (cons (:fn ast) (:args ast)))

    :fn
    (cons 'fn (concat (when-some [l (:local ast)] [(:name l)])
                (map emit-cljs-method (:methods ast))))

    :letfn
    (list 'letfn (into [] (map (fn [{:keys [name init]}]
                                 (cons name (map emit-cljs-method (:methods init)))))
                   (:bindings ast))
      (emit-cljs (:body ast)))

    :try
    `(~'try ~(emit-cljs (:body ast))
      ~@(when-not (= :throw (:op (:catch ast)))
          (let [name (get-in ast [:name])]
            [(list 'catch :default name (emit-cljs (:catch ast)))]))
      ~@(when-some [f (:finally ast)]
          [(list 'finally (emit-cljs f))]))

    :throw
    (list 'throw (emit-cljs (:exception ast)))

    :new
    (cons 'new (map emit-cljs (cons (:class ast) (:args ast))))

    :def
    (list 'def (emit-cljs (:var ast)) (emit-cljs (:init ast)))

    :set!
    (list 'set! (emit-cljs (:target ast)) (emit-cljs (:val ast)))

    :js
    (list* 'js* (or (:code ast) (apply str (interpose "~{}" (:segs ast)))) (map emit-cljs (:args ast)))

    :do
    (cons 'do (conj (mapv emit-cljs (:statements ast)) (emit-cljs (:ret ast))))

    :map
    (zipmap (map emit-cljs (:keys ast)) (map emit-cljs (:vals ast)))

    :set
    (into #{} (map emit-cljs) (:items ast))

    (:vec :vector)
    (into [] (map emit-cljs) (:items ast))

    :list `(list ~@ (map emit-cljs (:items ast)))

    :js-array `(cljs.core/array ~@(map emit-cljs (:items ast)))
    :js-object `(cljs.core/js-obj ~@(interleave (:keys ast) (map emit-cljs (:vals ast))))

    :if
    (list 'if (emit-cljs (:test ast)) (emit-cljs (:then ast)) (emit-cljs (:else ast)))

    :case
    (list* 'case (emit-cljs (:test ast))
      (-> []
        (into (mapcat (fn [{:keys [tests then]}]
                        [(map :form tests) (emit-cljs (:then then))]))
          (:nodes ast))
        (conj (emit-cljs (:default ast)))))

    :host-field
    (list '. (emit-cljs (:target ast)) (symbol (str "-" (name (:field ast)))))

    :host-call
    (list* '. (emit-cljs (:target ast)) (:method ast) (map emit-cljs (:args ast)))

    :with-meta `(with-meta ~(emit-cljs (:expr ast)) ~(emit-cljs (:meta ast)))

    :the-var `(var ~(-> ast :var :form))
    (:js-var :var :local :const :quote) (:form ast)


    ;; :binding   ; Handled in let and fn

    ;; :case-node ; Handled in :case
    ;; :case-test
    ;; :case-then

    ;; :fn-method ; Handled in fn

    ;; :no-op     ; Unknown use case

    ;; :defrecord ; Won’t be supported
    ;; :ns
    ;; :ns*


    (do (clojure.tools.logging/warn "This cljs form is not supported yet. Please log a ticket." {:op (:op ast) :form (:form ast)})
        (:form ast))))
