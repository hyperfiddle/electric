(ns hyperfiddle.hfql
  (:require #?(:clj [hyperfiddle.hfql.impl :as impl])
            #?(:clj [hyperfiddle.hfql.env :as env])
            #?(:clj [datahike.api :as d]
               :cljs [datascript.core :as d])
            #?(:clj [datahike.impl.entity :as de]
               :cljs [datascript.impl.entity :as de]))
  #?(:cljs (:require-macros [hyperfiddle.hfql :refer [hfql]])))

(defmacro hfql [form] (impl/hfql &env form))

#?(:clj (defn expand [&env form] (impl/parse (env/resolve-syms &env form))))

#?(:clj (defmacro render [& args] (apply impl/render* args)))

(defn nav!
  ([_ e] e)
  ([db e a] (let [v (a (if (de/entity? e) e (d/entity db e)))]
              (if (de/entity? v)
                (:db/id v)
                v)))
  ([db e a & as] (reduce (partial nav! db) (nav! db e a) as)))