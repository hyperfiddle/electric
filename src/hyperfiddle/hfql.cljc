(ns hyperfiddle.hfql
  (:require #?(:clj [hyperfiddle.hfql.impl :as impl])
            #?(:clj [hyperfiddle.hfql.env :as env])))

(defmacro hfql [form] (impl/hfql &env form))

#?(:clj (defn expand [&env form] (impl/parse (env/resolve-syms &env form))))

#?(:clj (defmacro render [& args] (apply impl/render* args)))