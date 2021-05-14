(ns hyperfiddle.rcf.unify-macro
  (:require [clojure.zip :as zip]))

(declare garner-unifiers occurs? bind-phase)

(defn ignore-variable? [sym] (= '_ sym))

(defn bind-phase
  [binds variable expr]
  (if (or (nil? expr)
          (ignore-variable? variable))
    binds
    (assoc binds variable expr)))

(def composite? seqable?)

(defn occurs?
  "Does v occur anywhere inside expr?"
  [variable? v expr binds]
  (loop [z (zip/zipper composite? seq #(do % %2) [expr])]
    (let [current (zip/node z)]
      (cond
        (zip/end? z) false
        (= v current) true
        (and (variable? current)
             (find binds current))
        (recur (zip/next (zip/insert-right z (binds current))))
        (zip/end? z) false
        :else (recur (zip/next z))))))

(defn- determine-occursness
  [want-occurs? variable? v expr binds]
  (if want-occurs?
    `(if (occurs? ~variable? ~v ~expr ~binds)
       (throw (ex-info (str "Cycle found in the path " ~expr) {:type "IllegalStateException"}))
       (bind-phase ~binds ~v ~expr))
    `(bind-phase ~binds ~v ~expr)))

(defmacro create-var-unification-fn
   [want-occurs?]
   (let [varp  (gensym)
         v     (gensym)
         expr  (gensym)
         binds (gensym)]
     `(fn ~'var-unify
        [~varp ~v ~expr ~binds]
        (if-let [vb# (~binds ~v)]
          (hyperfiddle.rcf.unify/garner-unifiers ~varp vb# ~expr ~binds)
          (if-let [vexpr# (and (~varp ~expr) (~binds ~expr))]
            (hyperfiddle.rcf.unify/garner-unifiers ~varp ~v vexpr# ~binds)
            ~(determine-occursness want-occurs? varp v expr binds))))))
