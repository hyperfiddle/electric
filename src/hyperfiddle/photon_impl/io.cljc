;; Facilities for encoding/decoding of photon protocol messages.
;; * Data frames can be arbitrary clojure data or photon failures. Serialization is done via transit json, the failure
;; error is preserved if it's an instance of `hyperfiddle.photon.Pending` or `hyperfiddle.photon.Cancelled`, otherwise
;; the error is logged and turned into an instance of `hyperfiddle.photon.Remote`.
;; * Control frames are vectors of signed integers. Serialization is the concatenation of the binary representation of
;; these numbers as fixed-length 32-bit, big endian.

(ns ^:no-doc hyperfiddle.photon-impl.io
  (:require [missionary.core :as m]
            [cognitect.transit :as t]
            [hyperfiddle.logger :as log]
            #?(:cljs [com.cognitect.transit.types]))
  (:import (missionary Cancelled)
           (hyperfiddle.photon Failure Pending Remote)
           #?(:clj (java.util.function Supplier))
           #?(:clj (java.nio ByteBuffer))
           #?(:clj (java.io ByteArrayInputStream ByteArrayOutputStream))
           #?(:clj (clojure.lang IReduceInit))))

#?(:cljs (extend-type com.cognitect.transit.types/UUID IUUID)) ; https://github.com/hyperfiddle/hyperfiddle/issues/728

(def write-opts
  {:handlers
   {Failure
    (t/write-handler
      (fn [_] "failure")
      (fn [x]
        (let [e (.-error ^Failure x)]
          (if (instance? Cancelled e)
            :cancelled
            (if (instance? Pending e)
              :pending
              (do (log/error e)
                  :remote))))))}})

(def read-opts
  {:handlers
   {"failure"
    (t/read-handler
      (fn [x]
        (case x
          :remote (Failure. (Remote.))
          :pending (Failure. (Pending.))
          :cancelled (Failure. (Cancelled.)))))}})

(def set-ints
  (partial reduce-kv
    (fn [r i n]
      (let [offset (bit-shift-left i 2)]
        #?(:clj  (.putInt ^ByteBuffer r offset n)
           :cljs (doto r (.setInt32 offset n)))))))

(defn encode-numbers
  "Encode a control frame to a binary segment."
  [xs]
  (let [required (bit-shift-left (count xs) 2)]
    #?(:clj (set-ints (ByteBuffer/allocate required) xs)
       :cljs (doto (js/ArrayBuffer. required)
               (-> (js/DataView.) (set-ints xs))))))

(defn decode-numbers
  "Decode a control frame from a binary segment."
  [b]
  (vec
    (reify
      #?(:clj IReduceInit :cljs IReduce)
      #?(:clj (reduce [_ rf r]
                (let [l (.limit ^ByteBuffer b)]
                  (loop [r r, i (int 0)]
                    (if (< i l)
                      (recur (rf r (.getInt ^ByteBuffer b i))
                        (unchecked-add-int i 4)) r))))
         :cljs (-reduce [_ rf r]
                 (let [l (.-byteLength b)
                       v (js/DataView. b)]
                   (loop [r r, i 0]
                     (if (< i l)
                       (recur (rf r (.getInt32 v i))
                         (+ i 4)) r))))))))

(defn encode
  "Encode a data frame to transit json"
  [x]
  #?(:clj (let [out (ByteArrayOutputStream.)]
            (t/write (t/writer out :json write-opts) x)
            (.toString out))
     :cljs (t/write (t/writer :json write-opts) x)))

(defn decode
  "Decode a data frame from transit json"
  [^String s]
  #?(:clj (t/read (t/reader (ByteArrayInputStream. (.getBytes s "UTF-8")) :json read-opts))
     :cljs (t/read (t/reader :json read-opts) s)))

(defn message-reader
  "Returns a task reading a photon message from provided task reading an individual frame."
  [?read]
  (m/sp
    (loop [msg []]
      (let [x (m/? ?read)]
        (if (string? x)
          (recur (conj msg
                   (try (doto (decode x) (->> (log/trace "🔽")))
                        (catch #?(:clj Throwable :cljs :default) t
                          (throw (ex-info "Failed to decode" {:value x} t))))))
          (conj msg (decode-numbers x)))))))

(defn message-writer
  "Returns a function taking a photon message and returning a task writing it as individual frames using provided
   function."
  [write]
  #(m/sp
     (loop [xs (seq (pop %))]
       (if-some [[x & xs] xs]
         (do (log/trace "🔼" x)
             (m/? (write
                    (try (encode x)
                         (catch #?(:clj Throwable :cljs :default) t
                           (throw (ex-info "Failed to encode" {:value x} t))))))
             (recur xs))
         (m/? (write (encode-numbers (peek %))))))))