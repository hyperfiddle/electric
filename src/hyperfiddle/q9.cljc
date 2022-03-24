(ns hyperfiddle.q9
  (:require #?(:clj [hyperfiddle.q9.impl :as impl])
            #?(:clj [hyperfiddle.q9.env :as env]))
  #?(:cljs (:require-macros [hyperfiddle.q9 :refer [hfql]])))

(defmacro hfql [form] #?(:clj (impl/hfql &env form)))
#?(:clj (defn expand [&env form] (impl/parse (env/resolve-syms &env form))))