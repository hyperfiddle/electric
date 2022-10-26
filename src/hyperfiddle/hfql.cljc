(ns hyperfiddle.hfql
  (:require #?(:clj [hyperfiddle.hfql.impl :as impl]))
  #?(:cljs (:require-macros [hyperfiddle.hfql])))

(defmacro hfql
  ([form] (impl/hfql* &env [] form))
  ([bindings form] (impl/hfql* &env bindings form)))

