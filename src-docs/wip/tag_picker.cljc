(ns wip.tag-picker
  (:require [clojure.string :as str]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-ui4 :as ui]))

(def data {:alice   {:name "Alice B"}
           :bob     {:name "Bob C"}
           :charlie {:name "Charlie D"}
           :derek   {:name "Derek E"}})
(defn q [search] (into [] (keep (fn [[k {nm :name}]] (when (str/includes? nm search) k))) data))

(e/defn TagPicker []
  (e/server
    (let [!v (atom #{:alice :bob})]
      (ui/tag-picker (e/watch !v)
        (e/fn [v] (e/client (prn [:V! v])) (swap! !v conj v))
        (e/fn [v] (prn [:unV! v]) (swap! !v disj v))
        (e/fn [search] (e/client (prn [:Options search])) (q search))
        (e/fn [id] (e/client (prn [:OptionLabel id])) (-> data id :name))))))
