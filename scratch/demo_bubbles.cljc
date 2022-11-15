(ns wip.demo-bubbles
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [contrib.str :refer [pprint-str]])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.demo-bubbles)))

(defn map-commands [f xs] (into [] (comp (map f) (remove nil?)) xs))

(defn command->statement [[tag value]] (case tag
                                         :set-checked value
                                         nil))

(p/defn App []
  (p/client
    (dom/div
      (dom/h1 (dom/text "Statement bubbling"))
      (dom/p (dom/text "Photon-dom elements collect and return statements."))
      (let [commands (dom/div
                       [:command "a command is a pair"]
                       (let [{:keys [::ui/value]} (ui/input {::ui/value "initial"})]
                         [:set-text value])
                       [[:command 1] [:command 2]] ; they can be grouped
                       (dom/ul
                         [:location :nested-in-ul] ; they bubble up from children dom nodes
                         (p/for [x [1 2 3]]
                           (dom/li (dom/label
                                     [:set-checked {:checkbox/id x
                                                    :checkbox/value (::ui/value (ui/checkbox))}]
                                     (dom/text " " x))))))]
        (dom/p (dom/text "Commands:"))
        (dom/pre
          (dom/text (pprint-str commands)))
        (dom/p (dom/text "Statements:"))
        (dom/pre
          (dom/text (pprint-str (map-commands command->statement commands))))))))

(def main
  #?(:cljs (p/boot
             (try
               (binding [dom/node (dom/by-id "root")]
                 (App.))
               (catch Pending _)))))

(comment
  #?(:clj (user/browser-main! `main))
  )
