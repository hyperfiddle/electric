(ns hyperfiddle.hfql
  (:require #?(:clj [hyperfiddle.hfql.impl :as impl])
            #?(:clj [hyperfiddle.hfql.env :as env])
            [datascript.core :as d]
            [datascript.impl.entity :as de]
            #?(:clj [hyperfiddle.rcf.analyzer :as ana]))
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

#?(:clj (defmethod ana/macroexpand-hook `hfql [_the-var _form _env args] `(hfql ~@args)))
