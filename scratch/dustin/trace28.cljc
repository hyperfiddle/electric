(ns dustin.trace28
  (:require [minitest :refer [tests]]
            [missionary.core :as m]))

(def via-fmap-ast '(let [>a (input)
                         >b (input)
                         >c (inc ~>a ~>b)]))

(def extended-fmap-ast '(let [>a (input)
                              >b (input)
                              >c (fmap clojure.core/+ >a >b)]))

(defmacro amb= [& forms]
  `(case (m/?= (m/enumerate (range ~(count forms))))
     ~@(interleave (range) forms)))


(defn gen-trace-pairs [syms]
  (for [sym syms]
    `{[~sym] (m/?? ~sym)}))

(tests
 (gen-trace-pairs '[>a >b]) := [{['>a] `(m/?? ~'>a)}
                                {['>b] `(m/?? ~'>b)}])

(defn trace! [tracef >effects]
  (m/stream! (m/ap (tracef (m/?? >effects)))))

(defn gen-trace [tracef syms]
  `(trace! ~tracef (m/stream! (m/relieve merge (m/ap (amb= ~@(gen-trace-pairs syms)))))))

(tests
 (gen-trace `identity '[>a >b])
 :=
 `(trace! identity (m/stream! (m/relieve merge (m/ap (amb= {[~'>a] (m/?? ~'>a)}
                                                           {[~'>b] (m/?? ~'>b)}))))))

(defn gen-input [inputs-registry sym]
  `[~sym (m/signal! (m/watch (get ~inputs-registry ~sym)))])

(tests
 (gen-input 'inputs '>a) := `[~'>a (m/signal! (m/watch (get ~'inputs ~'>a)))])


(defn gen-fmap [bound-name f & args]
  `[~bound-name (m/signal! (m/latest ~f ~@args))])

(tests
 (gen-fmap '>c `+ '>a '>b) := `[~'>c (m/signal! (m/latest + ~'>a ~'>b))])

(defn let-bindings [[_ bindings & body]]
  (partition 2 bindings))

(tests
 (let-bindings '(let [a 1, b 2] …)) := '[[a 1] [b 2]])

(defn gen-from-trace [bound-name trace-path >replayer]
  `[~bound-name (from-trace! '~trace-path ~>replayer)])

(defn translate-binding [input-registry passives >replayer [sym form]]
  (if (contains? passives sym)
    (gen-from-trace sym (get passives sym) >replayer)
    (case (first form)
      fmap  (apply gen-fmap sym (rest form))
      input (gen-input input-registry sym))))

(defn compile-functor-client [ast input-registry passives >replayer tracef]
  (let [bindings     (let-bindings ast)
        syms         (distinct (map first bindings)) ;; shadowing
        new-bindings (mapcat (partial translate-binding input-registry passives >replayer) bindings)]
    `(fn [~input-registry ~>replayer ~tracef]
       (let [~@new-bindings]
         ~(gen-trace tracef syms)))))

(declare from-trace!)

(tests
 (compile-functor-client extended-fmap-ast
                         'inputs
                         {'>a ['>a]
                          '>b ['>b]}
                         '>replayer
                         'tracef)
 :=
 `(fn [~'inputs ~'>replayer ~'tracef]
    (let [~'>a (from-trace! '[~'>a] ~'>replayer)
          ~'>b (from-trace! '[~'>b] ~'>replayer)
          ~'>c (m/signal! (m/latest + ~'>a ~'>b))]
      (trace! ~'tracef (m/stream! (m/relieve merge (m/ap (amb= {[~'>a] (m/?? ~'>a)}
                                                               {[~'>b] (m/?? ~'>b)}
                                                               {[~'>c] (m/?? ~'>c)}))))))))

