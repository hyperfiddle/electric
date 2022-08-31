(ns hyperfiddle.photon-impl.analyzer
  "Utilities to analyze and transform clojure/script code. Not photon code.
  Use case: support `clojure.core/fn` in a photon program.
  Long term goal is to build a unified clojure/script code walker and get rid of tools.analyzer deps."
  (:require [clojure.tools.analyzer.passes.emit-form :as emit-form]
            [clojure.tools.analyzer.ast :as clj-ast]
            [clojure.tools.analyzer.jvm :as clj]
            [cljs.analyzer.api :as cljs]
            [cljs.analyzer :as cljs-ana]
            [cljs.analyzer.passes :as cljs-ast]
            [hyperfiddle.logger :as log]))

(defn analyze-clj "Analyze a clj form to ast without any passes." [env form]
  (binding [clj/run-passes identity]
    (clj/analyze form env)))

(defn analyze-cljs "Analyze a cljs form to ast without any passes." [env form]
  (binding [cljs-ana/*passes* []]
    (cljs/analyze env form)))

(defn emit-clj [ast]
  (emit-form/emit-form ast))

(defn walk-clj "Prewalk a clj ast" [ast f] (clj-ast/prewalk ast f))
(defn walk-cljs "Prewalk a cljs ast" [ast f] (cljs-ast/walk ast [(fn [env ast opts] (f ast))]))

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
    (list* 'try (emit-cljs (:body ast))
      (list 'catch :default (:name ast) (emit-cljs (:catch ast)))
      (when-some [f (:finally ast)]
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


    (do (hyperfiddle.logger/warn "This cljs form is not supported yet. Please log a ticket." {:op (:op ast) :form (:form ast)})
        (:form ast))))
