(ns hyperfiddle.hfql2
  (:require #?(:clj [hyperfiddle.hfql2.impl :as impl])
            [hyperfiddle.photon :as p]
            [hyperfiddle.api :as-alias hf]
            [clojure.spec.alpha :as s])
  #?(:cljs (:require-macros [hyperfiddle.hfql2])))

(defmacro hfql
  ([form] (impl/hfql* &env [] form))
  ([bindings form] (impl/hfql* &env bindings form)))

(p/defn JoinArg [ctx-or-V]
  (if (map? ctx-or-V)
    (hf/JoinAllTheTree. ctx-or-V)
    (new ctx-or-V)))

(defn literal [collf & x] (collf x))    ; TODO rename

(s/fdef literal :args (s/cat :collf fn? :x any?) :ret any?)
