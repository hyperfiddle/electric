(ns user.demo-3-system-properties
  (:require [clojure.string :as str]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  #?(:cljs (:require-macros user.demo-3-system-properties)))

; A web view that queries the backend JVM environment and writes it to the
; frontend dom, all in a single composed expression.
; The p/for is stabilized with a "react key" for efficient DOM maintenance.

(defn system-properties [?s]
  #?(:clj (->> (System/getProperties)
               (filter (fn [[k v]] (str/includes? (str/lower-case (str k)) (str/lower-case (str ?s)))))
               (into {}))))

(p/defn App []
  (p/client
    (dom/div
      (dom/h1 "System Properties")
      (let [!search (atom ""), search (p/watch !search)]
        (p/server
          (let [system-props (sort-by key (system-properties search))
                matched-count (count system-props)]
            (p/client
              (dom/div {:style {:color "gray"}} matched-count " matches")
              (ui/input {::dom/type :search
                         ::dom/placeholder "java.home"
                         ::ui/input-event (p/fn [e] (reset! !search (:value dom/node)))})
              (dom/table
                (p/server
                  (p/for-by first [[k v] system-props]
                    (p/client
                      (dom/tr
                        (dom/td k)
                        (dom/td {:style {:white-space :nowrap}} v)))))))))))))
