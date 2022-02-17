(ns hyperfiddle.q8
  (:require #?(:clj [hyperfiddle.q8.impl :as impl])
            [hyperfiddle.q8.lib])
  #?(:cljs (:require-macros [hyperfiddle.q8 :refer [hfql]])))

(defmacro hfql [form] #?(:clj (impl/hfql &env form)))
