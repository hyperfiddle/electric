(ns hyperfiddle.wire
  (:require [clojure.core.async :as a]
            [hyperfiddle.fabric :as f]
            [minitest :refer [tests]])
  (:import java.util.UUID))

;;;;;;;;;;;;;;;;;
;; Direct ref  ;;
;;;;;;;;;;;;;;;;;

(tests
 ;; a <- b
 (do
   (def state (atom nil))
   (def a (f/pure 1))
   (def b (f/fmap inc a))
   (f/on b #(do (prn %) (reset! state %)))
   (Thread/sleep 100)
   nil) => nil
 @state => 2)

;;;;;;;;;;;;;;;;;;
;; INDIRECT REF ;;
;;;;;;;;;;;;;;;;;;

(tests
 ;; a <- ba <- bb
 (do
   (def state (atom nil))
   (def a (f/pure 1))
   (def ba (f/fmap inc a))
   (def bb (f/fmap identity ba))
   (f/on b #(reset! state %))
   nil)
 @state => 2)

;;;;;;;;;;;;;;;;
;; Manual REF ;;
;;;;;;;;;;;;;;;;

(tests
 ;; a <- ba <~ bb
 (do
   (def state (atom nil))
   (def a (f/input))
   (def ba (f/fmap inc a))

   (defn connect [>a]
     (let [>b (volatile! nil)]
       (vreset! >b (f/input (fn on []
                              (f/on >a (fn [x] (future (f/put @>b x))))
                              (fn off []
                                (f/off >a)))))
       @>b))

   (def bb (connect ba))
   (def out (f/on bb #(do (prn %) (reset! state %))))
   (f/put a 1)
   ;; (f/off out) ;; doesn’t work yet, missing detach call in DF.hx
   (Thread/sleep 200)
   nil) => nil
 @state => 2)

;;;;;;;;;;;;;;;;;;
;; VIA REGISTRY ;;
;;;;;;;;;;;;;;;;;;

(tests
 (do
   (def state (atom nil))
   (def registry (atom {}))

   ;; pure :: a -> R a
   (defn pure [node]
     (let [id (UUID/randomUUID)]
       (swap! registry assoc id node)
       id))

   (defn ask [id]
     (get @registry id))

   ;; fmap :: (a -> b) -> R a -> R b
   (defn fmap [f a]
     (pure (f/fmap f (ask a))))

   (def a (pure (f/pure 1)))
   (def b (fmap inc a))

   (f/on (ask b) #(reset! state %))
   nil) => nil

 @state => 2
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VIA REGISTRY, MANUAL ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(tests
 (do
   (def state (atom nil))
   (def registry (atom {}))

   (defn pure [node]
     (let [id (UUID/randomUUID)]
       (swap! registry assoc id node)
       id))

   (defn ask [id]
     (get @registry id))

   ;; fmap :: (a -> b) -> R a -> R b
   (defn fmap [f a]
     (pure (f/fmap f (ask a))))

   (def a (pure (f/pure 1)))
   (def b (fmap inc a))
   (def c (pure (connect (ask b))))

   (f/on (ask c) #(do (prn "on ask c" %) (reset! state %)))
   (Thread/sleep 300)
   nil) => nil

 @state => 2
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OVER WIRE, REGISTRY, MANUAL ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TBD

(defn send! [{:keys [up down] :as _wire} {:keys [type] :as message}]
  (let [chan (case type
               :subscribe up
               :publish   down)]
    (a/go (a/>! chan message))))

(defn subscribe! [wire IDa]
  (send! wire {:type :subscribe
               :id   IDa}))

(defn publish! [wire IDa data]
  (send! wire {:type :publish
               :id   IDa
               :data data}))

(defn make-wire []
  {:up   (a/chan)
   :down (a/chan)})

(def ^:dynamic *registry*)

(def server-registry (atom {}))
(def client-registry (atom {}))

(defmacro server [& body]
  `(binding [*registry* server-registry]
     ~@body))

(defmacro client [& body]
  `(binding [*registry* client-registry]
     ~@body))

(defn put! [IDa v]
  (f/put (get @*registry* IDa) v))

(defn register! [IDa >a]
  (swap! *registry* assoc IDa >a)
  IDa)

(defn lookup [IDa]
  (get @*registry* IDa))

(defn make-server [wire]
  (a/go-loop [{:keys [type id]} (a/<! (:up wire))]
    (case type
      :subscribe (f/on (lookup id) (fn [a] (publish! wire id a))))))

(defn make-client [wire]
  (a/go-loop [{:keys [type id data]} (a/<! (:down wire))]
    (case type
      :publish (put! id data))))

(defn connect-wire [wire IDa]
  (let [>b (f/input)]
    (register! IDa >b)
    (subscribe! wire IDa)
    IDa))

(tests
 (do
   (def wire (make-wire))
   (def a (server (register! 1 (f/input))))
   (def b (server (register! 2 (f/pure 2))))
   (def c (server (register! 3 (f/fmap + (lookup a) (lookup b)))))
   (def d (client (connect-wire wire c)))
   (def e (client (register! 4 (f/fmap inc (lookup d)))))

   (def client-end (client (f/cap (lookup e))))
   (server (make-server wire))
   (client (make-client wire))
   (server (f/put (lookup a) 1))

   (Thread/sleep 100)
   @client-end) => 4) ;; => 4 ✔
