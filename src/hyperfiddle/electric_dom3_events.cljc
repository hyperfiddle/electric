;; This ns only exists to prevent code duplication.
;; Required in dom3 and electric core (to pause Clock on DOM hidden)

(ns hyperfiddle.electric-dom3-events
  (:require [missionary.core :as m]))

#?(:cljs (defn with-listener
           ([n e f] (with-listener n e f nil))
           ([n e f o] (.addEventListener n e f o) #(.removeEventListener n e f o))))

#?(:cljs
   (defn listen "Takes the same arguments as `addEventListener` and returns an uninitialized
  missionary flow that handles the listener's lifecycle producing `(f e)`.
  Relieves backpressure. `opts` can be a clojure map."
     ([node event-type] (listen node event-type identity))
     ([node event-type f] (listen node event-type f {}))
     ([node event-type f opts]
      (->> (m/observe (fn [!] (with-listener node event-type #(! (f %)) (clj->js opts))))
        (m/relieve {})))))

#?(:cljs
   (defn listen-some "Takes the same arguments as `addEventListener` and returns an uninitialized
  missionary flow that handles the listener's lifecycle producing `(f e)` unless
  the result is `nil`. Relieves backpressure. `opts` can be a clojure map."
     ([node event-type] (listen-some node event-type identity))
     ([node event-type f] (listen-some node event-type f {}))
     ([node event-type f opts]
      (->> (m/observe (fn [!]
                        (let [! #(some-> (f %) !), opts (clj->js opts)]
                          (.addEventListener node event-type ! opts)
                          #(.removeEventListener node event-type ! opts))))
        (m/relieve {}))
      ;; alternative implementation
      #_(m/eduction (filter some?) (listen node typ f opts)))))


