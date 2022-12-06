(ns .
  (:require
    [contrib.str :refer [pprint-str]]
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom :as dom]
    [hyperfiddle.photon-ui2 :as ui])
  #?(:cljs (:require-macros .))
  (:import (hyperfiddle.photon Pending)))

(let [!x (atom "a") x (p/watch !x)]
  (->> (ui/input x)
       (reset! !x))
  nil) ; legal and works

(loop [x "a"]
  (recur (ui/input x))
  nil) ; illegal under current model, but matches the above legal structure

(do (p/with-cycle [x "a"]
      (ui/input x))
    nil) ; legal and works

(defn reset!-ret-nil [& args] (apply reset! args) nil)

(let [!x (atom "a") x (p/watch !x)]
  (->> (ui/input x)
       (reset!-ret-nil !x))) ; cute - value is handled, return nil
