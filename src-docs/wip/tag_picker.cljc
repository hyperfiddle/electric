(ns wip.tag-picker
  (:require [clojure.string :as str]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-ui4 :as ui])
  #?(:cljs (:require-macros [wip.tag-picker])))

(def data {:alice   {:name "Alice B"}
           :bob     {:name "Bob C"}
           :charlie {:name "Charlie D"}
           :derek   {:name "Derek E"}})
(defn q [search] (into [] (keep (fn [[k {nm :name}]] (when (str/includes? nm search) k))) data))

(p/defn App []
  (p/server
    (let [!v (atom #{:alice :bob})]
      (ui/tag-picker (p/watch !v)
        (p/fn [v] (p/client (prn [:V! v])) (swap! !v conj v))
        (p/fn [v] (prn [:unV! v]) (swap! !v disj v))
        (p/fn [search] (p/client (prn [:Options search])) (q search))
        (p/fn [id] (p/client (prn [:OptionLabel id])) (-> data id :name))))))
