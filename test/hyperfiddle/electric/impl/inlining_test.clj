(ns hyperfiddle.electric.impl.inlining-test
  (:require [clojure.test :as t]
            [hyperfiddle.rcf :as rcf]
            [hyperfiddle.electric.impl.lang3 :as lang]
            [hyperfiddle.electric.impl.runtime3 :as r]))

(defmacro all
  ([o] `(all {} ~o))
  ([env o]
   `(lang/expand-all
      (merge
        ~(assoc (if (:js-globals &env)
                  (assoc &env ::lang/peers {:client :cljs, :server :cljs}, ::lang/current :client)
                  {:locals &env, ::lang/peers {:client :clj, :server :clj}, ::lang/current :client})
           :ns '{:name 'hyperfiddle.electric.impl.inlining-test})
        ~env)
      ~o)))

(def ^{::lang/inlinable '{0 [[] (hyperfiddle.electric.impl.inlining-test/X 0)] 1 [[n] n]}} X nil)

;; TODONEXT
(rcf/tests
  (all '(X)) := '(let* [n 0] n)
  (all {::lang/no-inline true} '(X)) := `(::lang/call ((::lang/static-vars r/dispatch) '~'X ~'X))
  (all '(let* [X Y] (X))) := `(let* [~'X ~'Y] (::lang/call ((::lang/static-vars r/dispatch) '~'X ~'X))))
