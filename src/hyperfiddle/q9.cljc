(ns hyperfiddle.q9
  (:require #?(:clj [hyperfiddle.q9.impl :as impl]))
  #?(:cljs (:require-macros [hyperfiddle.q9 :refer [hfql]])))

(defmacro hfql [form] #?(:clj (impl/hfql &env form)))

