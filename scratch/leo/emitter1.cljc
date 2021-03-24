(ns leo.emitter1
  (:require [missionary.core :as m]
            [minitest :refer [tests]]
            [dustin.trace28 :refer [amb= trace! from-trace! let-bindings gen-from-trace gen-fmap]]
            [dustin.analyzer1 :refer [analyze-ast]]))

(defn translate-binding [passives >replayer [sym form]]
  (if (contains? passives sym)
    (gen-from-trace sym (get passives sym) >replayer)
    (case (first form)
      :fmap  (apply gen-fmap sym (rest form))
      `[~sym (m/signal! ~form)])))

[{:sym  'cross_auto_0
  :type :simple}]
(defn gen-trace-pairs [trace-items]
  (map-indexed (fn [id {:keys [sym type]}]
                 (case type
                   :simple
                   `{[~id] (m/?? ~sym)}

                   :nested
                   `(let [[k >v] (m/?= (m/enumerate (m/?? ~sym)))]
                      {[~(comment (path sym)) k] (m/?? >v)}))) trace-items))


(defn gen-trace [tracef trace-items]
  `(trace! ~tracef (m/stream! (m/relieve merge (m/ap (amb= ~@(gen-trace-pairs syms)))))))

(defn trace-items [analysis]
  (comment TODO))

(defn emit-dag [analysis replayer-sym tracef-sym]
  `(fn [~replayer-sym ~tracef-sym]
     (let [~@(comment TODO with ret)]
       ~(gen-trace tracef-sym (trace-items analysis)))))

(defn compile-ast [ast replayer-sym tracef-sym]
  (-> ast
    (analyze-ast {})
    (emit-dag replayer-sym tracef-sym)))

;; TODO FIX