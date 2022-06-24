(ns wip.crud
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [hyperfiddle.photon-ui :as ui])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.crud)))

(p/defn App []
  (dom/div
   (dom/span (dom/text "Filter prefix:"))
   (ui/input {})
   (dom/ul
    (p/for [x [{:fname "Hans", :lname "Emil"}]]
      (dom/li (dom/text (:lname x) ", " (:fname x)))))
   (dom/span (dom/text "Name:"))
   (ui/input {})
   (dom/span (dom/text "Surname:"))
   (ui/input {})
   (ui/button {} (dom/text "Create"))
   (ui/button {} (dom/text "Update"))
   (ui/button {} (dom/text "Delete"))))

(def main #?(:cljs (p/client (p/main
                              (try
                                (binding [dom/node (dom/by-id "root")]
                                  (App.))
                                (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
