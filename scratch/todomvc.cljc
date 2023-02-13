(ns hyperfiddle.examples.todomvc
  (:require [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [missionary.core :as m]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]])
  #?(:cljs (:require-macros
             [hyperfiddle.examples.todomvc :refer [head todo-list]])))

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

(def events
  ; return seq of txs that are new since last time checked
  (comp (diff
          (fn [x y]
            (loop [e () x x]
              (if (= x y)
                e (recur (cons (next x) e)
                    (first x))))) nil) cat cat))

(defn build-eav [eav [op & args]]
  (case op
    :remove (dissoc eav (first args))
    :append (apply update eav (first args) assoc :done false :name (next args))
    :update (apply update eav (first args) assoc (next args))))

(def next-id!
  (let [c (atom 0)]
    (fn [_] (swap! c inc))))

(tests
  ((m/ap (! (z/pick (m/seed [1 2 3])))) #() #())
  % := 1

  ((m/ap (! (z/pick (m/amb>)))) #() #())
  % := ::rcf/timeout)

(tests
  "missionary idiom to return an empty flow"
  ((m/ap (! (m/amb>))) #() #())
  % := ::rcf/timeout

  ((m/ap (! (m/?< (m/seed [])))) #() #())
  % := ::rcf/timeout)

;; the staging area is a continuous value, represents the user intent.
;; it starts empty, meaning we prompt the user for a request.
;; in reaction to user interaction, the staging area is filled.
;; the caller of the widget is in charge of processing the request and looping back the result in the widget input.
;; the widget clears the staging area when it detects the change in input.

(defn stage "State machine starting with idle, then taking the value of the first event emitted by >event,
 then returning back to idle when the predicate returned by >check validates the event."
  [idle >check >events]
  ; impulse
  (m/ap
    (loop []
      (m/amb> idle
        (let [event (z/pick >events)]
          (m/amb> event
            ; wait for the pred to become true for this event
            ; wait until the entity is in the index
            (do (z/pick (m/eduction (filter #(% event)) >check))
                (recur))))))))

(comment
  (def !eav (atom #{}))
  ((m/reduce conj (stage nil (m/watch !eav) (m/observe (fn [!] (def event! !) #())))) ! !)
  (event! 1)
  (swap! !eav conj 1)
  ; impulse
  % := [nil 1 nil])


(p/def head)
(p/def todo-list
  #'(dom/div
      (let [view (atom :all)
            eav ~(->> #'head
                   (m/eduction events)
                   (m/reductions build-eav (sorted-map))
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
                                  (map next-id!))
                                (stage nil #'eav))]
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
                                             (stage nil #'(partial = done?)))]
                          [[:update id :done done?]]))
                      (dom/span
                        (dom/set-text-content! dom/parent (:name (eav id))))
                      (dom/input
                        (dom/set-attribute! dom/parent "type" "button")
                        (dom/set-attribute! dom/parent "value" "remove")
                        (when-some [id ~(->> (dom/events dom/parent dom/click-event)
                                          (m/eduction (map (constantly id)))
                                          (stage nil #'(complement eav)))]
                          [[:remove id]]))))))))
          (dom/div
            (dom/span
              (dom/set-text-content! dom/parent (str (count active) " items left" ))))))))

(def log (atom nil))

(defn todo-mvc []
  (p/run
    (binding [head ~(m/watch log)
              dom/parent (dom/by-id "todomvc")]
      (when-some [txs (seq ~todo-list)]
        (swap! log cons txs)))))

#?(:cljs (todo-mvc))
