(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))

(hyperfiddle.rcf/enable!)

(tests
  (macroexpand '(p/def db (inc 42)))
  := '(def db) ; macroexpands to nothing

  (p/def db (inc 42))
  ; , the (inc 42) gets quoted and stored as meta
  (meta *1) := {:hyperfiddle.photon-impl.compiler/node '(inc 42),
                :name 'db,
                :line _, :column _, :file _, :ns _})

(tests
  "p/fn is only defined in a photon block, if you eval from clojure
  you get weird stuff that isn't defined from clojure"
  (macroexpand '(p/fn [x] x))
  := (list
       :hyperfiddle.photon-impl.compiler/closure
       '(clojure.core/let [x hyperfiddle.photon-impl.compiler/%1] x)
       {:hyperfiddle.photon.debug/name nil,
        :hyperfiddle.photon.debug/args ['x],
        :hyperfiddle.photon.debug/type :reactive-fn,
        :line _}))
