(ns wip.photon-tree
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros wip.photon-tree)))

(def tree '{:deps     true
            :builds   {:app {:target     :browser
                             :asset-path "/js"
                             :output-dir "resources/public/js"
                             :modules    {:main {:init-fn user/start!
                                                 :entries [user]}}
                             :devtools   {:watch-dir       "resources/public"
                                          :hud             #{:errors :progress}
                                          :ignore-warnings true}}}
            :dev-http {8080 "resources/public"}
            :nrepl    {:port 9001}
            :npm-deps {:install false}})


(p/def View)
(p/def CollView)
(p/def MapEntryView)
(p/def MapView)

(defn create-toggle
  [a]
  (fn [e]
    (prn :toggle)
    (.stopPropagation e)
    (swap! a not)))

(p/defn App
  []
  ;; use dynamic bindings because the compiler doesn't support mutual recursion
  ;; in `p/defn` yet
  (binding [View (p/fn [data]
                   (cond
                     (map? data) (MapView. data)
                     (coll? data) (CollView. data)
                     :else (dom/text (pr-str data))))
            CollView (p/fn [data]
                       (let [[begin end] (cond
                                           (vector? data) "[]"
                                           (set? data) ["#{" "}"]
                                           :else "()")
                             *expanded? (atom false)
                             toggle (create-toggle *expanded?)]
                         (dom/li
                           (dom/attribute "role" "treeitem")
                           (new (->> (dom/events dom/parent "click")
                                     (m/eduction (map toggle))

                                     (p/continuous)))
                           (dom/ul
                             (dom/attribute "class" "view coll-view")
                             (dom/attribute "role" "group")
                             (if (p/watch *expanded?)
                               (dom/attribute "aria-expanded" "true")
                               (dom/attribute "aria-expanded" "false"))
                             (dom/text begin)
                             (dom/for [x data]
                                      (dom/li
                                        (dom/attribute "role" "treeitem")
                                        (View. x)))
                             (dom/text end)))))
            MapEntryView (p/fn [k v]
                           (dom/li
                             (dom/attribute "role" "treeitem")
                             (dom/ul
                               (dom/attribute "class" "view map-entry-view")
                               (dom/attribute "role" "group")
                               (View. k)
                               (View. v))))
            MapView (p/fn [data]
                      (let [*expanded? (atom false)
                            toggle (create-toggle *expanded?)]
                        (dom/li
                          (dom/attribute "role" "treeitem")
                          (new (->> (dom/events dom/parent "click")
                                    (m/eduction (map toggle))
                                    (p/continuous)))
                          (dom/ul
                            (dom/attribute "class" "view map-view")
                            (dom/attribute "role" "group")
                            (if (p/watch *expanded?)
                              (dom/attribute "aria-expanded" "true")
                              (dom/attribute "aria-expanded" "false"))
                            (dom/text "{")
                            (dom/for [e data]
                                     (MapEntryView. (key e) (val e)))
                            (dom/text "}")))))]
    (dom/div
      (dom/h1 (dom/text "Tree view"))
      (dom/ul
        (dom/attribute "class" "view")
        (dom/attribute "role" "tree")
        (View. tree)))))
