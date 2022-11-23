(ns dustin.y2022.state-2
  (:require #?(:cljs goog.events)
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [missionary.core :as m])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.state-2)))

(defmacro with-cycle [[s i] & body]
  `(let [a# (atom ~i) ~s (p/watch a#)]
     (reset! a# (do ~@body))))

(defn happen [s e]
  (case (:status s)
    :idle {:status :impulse :event e} s)) ; if not idle, event is lost

; data EventState = Idle | Impulse event | Pending event
(p/defn Event [type busy]
  (:event
    (let [!state (atom {:status :idle})
          state (p/watch !state)]
      (dom/event type (partial swap! !state happen)) ; discrete! this is the event wrapped into impulse
      (reset! !state
              (case (:status state)
                :idle state
                :impulse (assoc state :status :pending) ; impulse is seen for 1 frame and then cleared
                :pending (if busy state {:status :idle}))))))

(p/defn Button [label busy]
  (dom/with
    (dom/dom-element dom/node "button")
    (dom/set-text-content! dom/node label)
    (Event. "click" busy)))

(defn events* [dom-node event-name]
  (m/observe (fn [!] (.addEventListener dom-node event-name !)
               #(.removeEventListener dom-node event-name !))))

(defmacro events [event-name]
  `(new (m/reductions {} nil (events* dom/node ~event-name))))

(p/defn EventBroken [type busy]
  (:event
    (with-cycle [s {:status :idle}]
      (let [e (events type)] ; in CT this event is never cleared, causing a loop
        (case (:status s)
          :idle (if (some? e) {:status :impulse :event e} s)
          :impulse (assoc s :status :pending)
          :pending (if busy s {:status :idle}))))))

(p/defn Demo []
  (p/client
    (with-cycle [busy false]
      (let [e (Button. "inc" busy)]
        (if (some? e)
          (try (p/server (Thread/sleep 500) false)
               (catch Pending _ true))
          busy)))))

;(tests
;  (with (p/run (tap
;                 (with-cycle [busy false]
;                   (when-let [_ (Button. "inc" busy)]
;                     (try (p/server (Thread/sleep 100) false)
;                          (catch Pending _ true))))))
;    ))
