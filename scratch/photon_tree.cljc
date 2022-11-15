(ns wip.photon-tree
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
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

#?(:cljs (defn stopPropagation [e] (.stopPropagation e))) ; Photon doesn't have interop syntax yet

(p/defn App
  []
  (dom/link {:rel :stylesheet, :href "photon_tree.css"})
  ;; use dynamic bindings because the compiler doesn't support mutual recursion
  ;; in `p/defn` yet
  (binding [View (p/fn [data]
                   (cond
                     (map? data) (MapView. data)
                     (coll? data) (CollView. data)
                     :else (pr-str data)))
            CollView (p/fn [data]
                       (let [[begin end] (cond
                                           (vector? data) "[]"
                                           (set? data) ["#{" "}"]
                                           :else "()")
                             *expanded? (atom false)]
                         (ui/element dom/li {::dom/role "treeitem"
                                             ::ui/click-event (p/fn [e]
                                                                (prn :toggle)
                                                                (stopPropagation e)
                                                                (swap! *expanded? not))}
                           (dom/ul {::dom/class "view coll-view"
                                    ::dom/role "group"
                                    ::dom/aria-expanded (p/watch *expanded?)}
                             begin
                             (p/for [x data]
                               (dom/li {::dom/role "treeitem"}
                                 (View. x)))
                             end))))
            MapEntryView (p/fn [k v]
                           (dom/li {::dom/role "treeitem"}
                             (dom/ul {::dom/class "view map-entry-view"
                                      ::dom/role "group"}
                               (View. k) " " (View. v))))
            MapView (p/fn [data]
                      (let [*expanded? (atom false)]
                        (ui/element dom/li {::dom/role "treeitem"
                                            ::ui/click-event (p/fn [e]
                                                               (prn :toggle)
                                                               (stopPropagation e)
                                                               (swap! *expanded? not))}
                          (dom/ul {::dom/class "view map-view"
                                   ::dom/role "group"
                                   ::dom/aria-expanded (p/watch *expanded?)}
                            "{" (p/for [e data]
                                  (MapEntryView. (key e) (val e)))
                            "}"))))]
    (dom/div
      (dom/h1 "Tree view")
      (dom/ul {::dom/class "view" ::dom/role "tree"}
        (View. tree)))))
