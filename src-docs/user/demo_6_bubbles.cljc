(ns user.demo-6-bubbles
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [clojure.pprint :as pprint])
  (:import (hyperfiddle.photon Pending)
           (missionary Cancelled))
  #?(:cljs (:require-macros user.demo-6-bubbles)))

(defn pprint-str [x]
  (with-out-str
    (pprint/with-pprint-dispatch pprint/code-dispatch
      (pprint/pprint x))))

(defn map-commands [f xs] (into [] (comp (map f) (remove nil?)) xs))

(defn command->statement [[tag value]] (case tag
                                         :set-checked value
                                         :intercepted {:input/value value}
                                         nil))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Statement bubbling"))
    (dom/p (dom/text "Photon-dom elements collect and return statements."))
    (let [commands (dom/div
                     [:statement "a statement is a pair"]
                     (let [[_tag value] (::ui/value (ui/input {:value     "initial"
                                                               ::ui/value (p/fn [value]
                                                                            ;; we can return them from event callbacks
                                                                            [:input-value value])}))]
                       [:intercepted value])
                     [[:statement 1] [:statement 2]] ; they can be grouped
                     (dom/ul
                       [:location :nested-in-ul] ; they bubble up from children dom nodes
                       (p/for [x [1 2 3]]
                         (dom/li (dom/label
                                   (ui/checkbox {::ui/value (p/fn [checked?]
                                                              [:set-checked {:checkbox/id      x
                                                                             :checkbox/checked checked?}])})
                                   (dom/text " " x))))))]
      (dom/p (dom/text "Statements:"))
      (dom/pre
        (dom/text (pprint-str commands)))
      (dom/p (dom/text "Transaction:"))
      (dom/pre
        (dom/text (pprint-str (map-commands command->statement commands)))))))

(def main #?(:cljs (p/client (p/main
                               (try
                                 (binding [dom/node (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _)
                                 (catch Cancelled _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
