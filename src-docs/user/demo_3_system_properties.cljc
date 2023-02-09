(ns user.demo-3-system-properties
  #?(:cljs (:require-macros user.demo-3-system-properties))
  (:require
   [clojure.string :as str]
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom2 :as dom]
   [hyperfiddle.photon-ui4 :as ui]))

; A web view that queries the backend JVM environment and writes it to the
; frontend dom, all in a single composed expression.
; The p/for is stabilized with a "react key" for efficient DOM maintenance.

(defn system-properties [?s]
  #?(:clj (->> (System/getProperties)
               (filter (fn [[k v]] (str/includes? (str/lower-case (str k)) (str/lower-case (str ?s)))))
               (into {}))))

(p/defn App []
  (p/client
    (dom/h1 (dom/text "System Properties"))
    (let [!search (atom ""), search (p/watch !search)]
      (p/server
        (let [system-props (sort-by key (system-properties search))
              matched-count (count system-props)]
          (p/client
            (dom/div (dom/props {:style {:color "gray"}}) (dom/text matched-count " matches"))
            (ui/input search (p/fn [v] (reset! !search v))
                      (dom/props {:type "search" :placeholder "java.home"}))
            (dom/table
              (p/server
                (p/for-by first [[k v] system-props]
                  (p/client
                    (dom/tr
                      (dom/td (dom/text k))
                      (dom/td (dom/props {:style {:white-space :nowrap}}) (dom/text v)))))))))))))
