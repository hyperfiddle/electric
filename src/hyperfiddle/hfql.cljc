(ns hyperfiddle.hfql
  (:require #?(:clj [hyperfiddle.hfql.impl :as impl])
            #?(:clj [hyperfiddle.hfql.env :as env]))
  #?(:cljs (:require-macros [hyperfiddle.hfql :refer [hfql]])))

(defmacro hfql [form] #?(:clj (impl/hfql &env form)))
#?(:clj (defn expand [&env form] (impl/parse (env/resolve-syms &env form))))