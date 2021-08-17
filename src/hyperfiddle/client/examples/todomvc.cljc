(ns hyperfiddle.client.examples.todomvc
  (:require [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [devcards.core :as dc :include-macros true]
            [hyperfiddle.client.examples.card :refer [dom-node]]
            [hyperfiddle.todomvc :as t])
  #?(:cljs (:require-macros
             [hyperfiddle.client.examples.todomvc :refer [head todo-list]])))

(p/def head)
(p/def todo-list
  #'(dom/div
      (let [view (atom :all)
            eav ~(->> #'head
                   (m/eduction t/events)
                   (m/reductions t/build-eav (sorted-map))
                   (m/relieve {}))
            active (remove (comp :done eav) (keys eav))
            completed (filter (comp :done eav) (keys eav))]
        (concat
          (dom/div
            (dom/input
              (dom/set-attribute! dom/parent "type" "text")
              (when-some [id ~(->> (dom/events dom/parent dom/keydown-event)
                                (m/eduction
                                  (filter (comp #{dom/keycode-enter} dom/keycode))
                                  (map t/next-id!))
                                (t/stage nil #'eav))]
                [[:append id (dom/get-value dom/parent)]])))
          (dom/div
            (apply concat
              (p/for [id (case ~(m/watch view)
                           :all (keys eav)
                           :active active
                           :completed completed)]
                (let [done? (:done (eav id))]
                  (dom/div
                    (concat
                      (dom/input
                        (dom/set-attribute! dom/parent "type" "checkbox")
                        (dom/set-checked! dom/parent done?)
                        (when-some [done? ~(->> (dom/events dom/parent dom/input-event)
                                             (m/eduction (map dom/event-target) (map dom/get-checked))
                                             (t/stage nil #'(partial = done?)))]
                          [[:update id :done done?]]))
                      (dom/span
                        (dom/set-text-content! dom/parent (:name (eav id))))
                      (dom/input
                        (dom/set-attribute! dom/parent "type" "button")
                        (dom/set-attribute! dom/parent "value" "remove")
                        (when-some [id ~(->> (dom/events dom/parent dom/click-event)
                                          (m/eduction (map (constantly id)))
                                          (t/stage nil #'(complement eav)))]
                          [[:remove id]]))))))))
          (dom/div
            (dom/span
              (dom/set-text-content! dom/parent (str (count active) " items left" ))))))))

(def log (atom nil))

(dc/defcard todomvc
  "# TodoMVC"
  (dom-node
    (fn [_ node]
      (p/run
        (p/binding [head ~(m/watch log)
                    dom/parent node]
          (when-some [txs (seq ~todo-list)]
            (swap! log cons txs)))))))
