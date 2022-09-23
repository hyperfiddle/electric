(ns hyperfiddle.hfql2
  (:require #?(:clj [hyperfiddle.hfql2.impl :as impl])
     [datascript.core :as d]
     [datascript.impl.entity :as de]  ; for `entity?` predicate
     )
  #?(:cljs (:require-macros [hyperfiddle.hfql2])))


(defmacro hfql [form] (impl/hfql* &env form))

(defn nav!
  ([_ e] e)
  ([db e a] (let [v (a (if (de/entity? e) e (d/entity db e)))]
              (if (de/entity? v)
                (:db/id v)
                v)))
  ([db e a & as] (reduce (partial nav! db) (nav! db e a) as)))

