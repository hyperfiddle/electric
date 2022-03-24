(ns hyperfiddle.q8
  (:require #?(:clj [hyperfiddle.q8.impl :as impl])
            #?(:clj [hyperfiddle.q8.reader])
            [hyperfiddle.q8.lib])
  #?(:cljs (:require-macros [hyperfiddle.q8 :refer [hfql]])))

(defmacro hfql [form] #?(:clj (impl/hfql &env form)))

