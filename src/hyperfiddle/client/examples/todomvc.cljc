(ns hyperfiddle.client.examples.todomvc
  (:require [datascript.core :as d]
            [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests ! %]]
            [missionary.core :as m]
            [devcards.core :as dc]
            [hyperfiddle.client.examples.card :refer [dom-node]])
  #?(:cljs (:require-macros
             [hyperfiddle.client.examples.todomvc :refer [pick head todo-list]])))

(p/defn todos [db status]
  (d/q '[:find [?e ...] :in $ ?status
         :where [?e :task/status ?status]]
    db status))

(defn diff [- z]
  (fn [rf]
    (let [state (doto (object-array 1)
                  (aset (int 0) z))]
      (fn
        ([r] (rf r))
        ([r x]
         (let [y (aget state (int 0))]
           (aset state (int 0) x)
           (rf r (- x y))))))))

(defn patch [+ z]
  (fn [rf]
    (let [state (doto (object-array 1)
                  (aset (int 0) z))]
      (fn
        ([r] (rf r))
        ([r x]
         (->> x
           (+ (aget state (int 0)))
           (aset state (int 0))
           (rf r)))))))

(def events
  (comp (diff
          (fn [x y]
            (loop [e () x x]
              (if (= x y)
                e (recur (cons (next x) e)
                    (first x))))) nil) cat cat))

(defn build-ids [ids [op & args]]
  (case op
    :remove (disj ids (first args))
    :append (conj ids (first args))
    ids))

(defn build-eav [eav [op & args]]
  (case op
    :remove (dissoc eav (first args))
    :append (apply update eav (first args) assoc :done false :name (next args))
    :update (apply update eav (first args) assoc (next args))))

(def first-or (partial m/reduce (comp reduced {})))

(defmacro pick [>f]
  `(let [x# (m/? (first-or ::empty ~>f))]
     (case x# ::empty (m/amb>) x#)))

;; the staging area is a continuous value, represents the user intent.
;; it starts empty, meaning we prompt the user for a request.
;; in reaction to user interaction, the staging area is filled.
;; the caller of the widget is in charge of processing the request and looping back the result in the widget input.
;; the widget clears the staging area when it detects the change in input.

(defn stage "State machine starting with idle, then taking the value of the first event emitted by >event,
 then returning back to idle when the predicate returned by >check validates the event."
  [idle >check >events]
  (m/ap
    (loop []
      (m/amb> idle
        (let [event (pick >events)]
          (m/amb> event
            (do (pick (m/eduction (filter #(% event)) >check))
                (recur))))))))

(def next-id!
  (let [c (atom 0)]
    (fn [_] (swap! c inc))))

(def p #(doto % prn))

(p/def head)
(p/def todo-list
  #'(dom/div
      (let [view (atom :all)
            all ~(->> #'head
                   (m/eduction events)
                   (m/reductions build-ids (sorted-set))
                   (m/relieve {}))
            eav ~(->> #'head
                   (m/eduction events)
                   (m/reductions build-eav {})
                   (m/relieve {}))
            active (remove (comp :done eav) all)
            completed (filter (comp :done eav) all)]
        (concat
          (dom/div
            (dom/input
              (dom/set-attribute! dom/parent "type" "text")
              (when-some [id ~(->> (dom/events dom/parent dom/keydown-event)
                                (m/eduction
                                  (filter (comp #{dom/keycode-enter} dom/keycode))
                                  (map next-id!))
                                (stage nil #'all))]
                [[:append id (dom/get-value dom/parent)]])))
          (dom/div
            (apply concat
              (p/for [id (case ~(m/watch view)
                           :all all
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
                                             (stage nil #'(partial = done?)))]
                          [[:update id :done done?]]))
                      (dom/span
                        (dom/set-text-content! dom/parent (:name (eav id))))
                      (dom/input
                        (dom/set-attribute! dom/parent "type" "button")
                        (dom/set-attribute! dom/parent "value" "remove")
                        (when-some [id ~(->> (dom/events dom/parent dom/click-event)
                                          (m/eduction (map (constantly id)))
                                          (stage nil #'(complement all)))]
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
