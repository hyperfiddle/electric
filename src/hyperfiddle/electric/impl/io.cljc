;; Facilities for encoding/decoding of Electric protocol messages.
;; * Data frames can be arbitrary clojure data or Electric failures. Serialization is done via transit json, the failure
;; error is preserved if it's an instance of `hyperfiddle.electric.Pending` or `hyperfiddle.electric.Cancelled`, otherwise
;; the error is logged and turned into an instance of `hyperfiddle.electric.Remote`.
;; * Control frames are vectors of signed integers. Serialization is the concatenation of the binary representation of
;; these numbers as fixed-length 32-bit, big endian.

(ns ^:no-doc hyperfiddle.electric.impl.io
  (:require [missionary.core :as m]
            [cognitect.transit :as t]
            #?(:clj [clojure.tools.logging :as log])
            [hyperfiddle.electric.debug :as dbg]
            [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
            #?(:cljs [com.cognitect.transit.types])
            [hyperfiddle.electric.impl.array-fields :as a])
  (:import (missionary Cancelled)
           (hyperfiddle.electric Failure Pending Remote FailureInfo)
           #?(:clj (java.nio ByteBuffer))
           #?(:clj (java.io ByteArrayInputStream ByteArrayOutputStream))
           #?(:clj (clojure.lang IReduceInit))))

#?(:cljs (extend-type com.cognitect.transit.types/UUID IUUID)) ; https://github.com/hyperfiddle/hyperfiddle/issues/728

(def default-write-handler ; Intercepts unserializable values, logs and return nil
  (t/write-handler ; Adapted from `com.cognitect.transit.impl.WriteHandlers.NullWriteHandler`
    (fn [x]
      (def -last-unserializable-for-repl x)
      (#?(:clj log/info, :cljs js/console.log) "Unserializable reference transfer:" (pr-str (type x)) (str x))
      "_")
    (fn [x] nil)
    (fn [_] "")))

(defn ->cache "Builds a minimal, cljc map/bounded-queue cache.
  One slot per key (map).
  Reaching `size` pops oldest value (bounded-queue)." [size]
  (doto (object-array (inc (* size 2))) (a/set (* size 2) 0)))
(defn cache-add [cache k v]
  (when-not (loop [i 0]
              (when (< i (dec (count cache)))
                (if (= k (a/get cache i))
                  (do (a/set cache (inc i) v) true)
                  (recur (+ i 2)))))
    (let [widx (a/getswap cache (dec (count cache)) #(mod (+ % 2) (dec (count cache))))]
      (a/set cache widx k, (inc widx) v))))
(defn cache-get [cache k]
  (loop [i 0]
    (when (< i (dec (count cache)))
      (if (= k (a/get cache i))
        (a/get cache (inc i))
        (recur (+ i 2))))))
(defn cache->map [cache]
  (loop [i 0, ac (transient {})]
    (if (< i (dec (count cache)))
      (recur (+ i 2) (assoc! ac (a/get cache i) (a/get cache (inc i))))
      (persistent! ac))))

(tests "keyed cache"
  (def !c (->cache 1))
  (cache-add !c 1 2) (cache-get !c 1) := 2
  (cache-add !c 1 3) (cache-get !c 1) := 3
  (cache-add !c 2 4) (cache-get !c 2) := 4
  (cache->map !c) := {2 4}

  "size 2"
  (def !c (->cache 2))
  (cache-add !c 1 1)
  (cache-add !c 2 2)
  (cache-add !c 2 2)
  (cache->map !c) := {1 1, 2 2})

(def !ex-cache (->cache 16))
(defn save-original-ex! [fi]
  (let [id (dbg/ex-id fi)]
    (when-some [cause (ex-cause fi)]
      (when-not (instance? FailureInfo cause)
        (cache-add !ex-cache id cause)))
    id))
(defn get-original-ex [id] (cache-get !ex-cache id))

(def ^:dynamic *write-handlers* nil)

(def failure-writer (t/write-handler
                      (fn [_] "failure")
                      (fn [x]
                        (let [err (.-error ^Failure x)]
                          (cond (instance? Cancelled err) [:cancelled]
                                (instance? Pending err)   [:pending]
                                (instance? Remote err)    [:remote (dbg/serializable (ex-data err))]
                                :else                     [:exception (ex-message err) (dbg/serializable (ex-data err))
                                                           (save-original-ex! err)])))))

(defn write-opts []
  {:handlers (merge *write-handlers*
               {Failure failure-writer
                :default default-write-handler}) ; cljs
   :default-handler default-write-handler}) ; clj

(def ^:dynamic *read-handlers* nil)

(def failure-reader (t/read-handler
                      (fn [[tag & args]]
                        (case tag
                          :exception (let [[message data id] args]
                                       (Failure. (dbg/ex-info* message data id nil)))
                          :remote    (let [[data] args]
                                       (Failure. (dbg/ex-info* "Remote error" (or data {}))))
                          :pending   (Failure. (Pending.))
                          :cancelled (Failure. (Cancelled.))))))

(defn read-opts [] {:handlers (merge *read-handlers* {"failure" failure-reader})})

(def set-ints
  (partial reduce-kv
    (fn [r i n]
      (let [offset (bit-shift-left i 2)]
        #?(:clj  (.putInt ^ByteBuffer r offset n)
           :cljs (doto r (.setInt32 offset n)))))))

(defn encode-numbers
  "Encode a control frame to a binary segment."
  [xs]
  (let [required (bit-shift-left (count xs) 2)] ; size of bytebuffer is 4 × (count xs), so shift by 2
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


;; #?(:cljs (def transit-writer (t/writer :json (write-opts))))
#?(:cljs (let [!cache (atom {:write-handlers *write-handlers*, :writer nil})]
           (defn transit-writer []
             (:writer (swap! !cache (fn [{:keys [write-handlers writer] :as cache}]
                                      (if (= write-handlers *write-handlers*)
                                        (if writer
                                          cache
                                          (assoc cache :writer (t/writer :json (write-opts))))
                                        {:write-handlers *write-handlers*
                                         :writer         (t/writer :json (write-opts))})))))))

(defn encode
  "Encode a data frame to transit json"
  [x]
  #?(:clj (let [out (ByteArrayOutputStream.)]
            (t/write (t/writer out :json (write-opts)) x)
            (.toString out))
     :cljs (t/write (transit-writer) x)))

;; #?(:cljs (def transit-reader (t/reader :json (read-opts))))
#?(:cljs (let [!cache (atom {:read-handlers *read-handlers*, :reader nil})]
           (defn transit-reader []
             (:reader (swap! !cache (fn [{:keys [read-handlers reader] :as cache}]
                                      (if (= read-handlers *read-handlers*)
                                        (if reader
                                          cache
                                          (assoc cache :reader (t/reader :json (read-opts))))
                                        {:read-handlers *read-handlers*
                                         :reader        (t/reader :json (read-opts))})))))))


(defn decode
  "Decode a data frame from transit json"
  [^String s]
  #?(:clj (t/read (t/reader (ByteArrayInputStream. (.getBytes s "UTF-8")) :json (read-opts)))
     :cljs (t/read (transit-reader) s)))

(defn decode-str [x]
  (try (doto (decode x) (->> (#?(:clj log/trace, :cljs js/console.debug) "🔽")))
    (catch #?(:clj Throwable :cljs :default) t
      (throw (ex-info "Failed to decode" {:value x} t)))))

(tests "FailureInfo"
  (def cause (ex-info "boom" {}))
  (def ex (dbg/ex-info* "x" {} cause))
  (def sent (-> ex Failure. encode decode .-error))
  "keeps the ID across the wire"
  (dbg/ex-id ex) := (dbg/ex-id sent)
  "can restore cause"
  (get-original-ex (dbg/ex-id sent)) := cause
  nil)

; Jetty rejects websocket payloads larger than 65536 bytes by default
; We’ll chop messages if needed
(def chunk-size (bit-shift-right 65536 2))

(defn ^:deprecated message-reader [?read]
  "Returns a discrete flow of read Electric messages from provided task, emitting individual frames."
  (m/sp
    (loop [data (transient [])]
      (let [x (m/? ?read)]
        (if (string? x)
          (recur (conj! data (decode-str x)))
          (persistent!
            (conj! data
              (loop [x       x
                     control (transient [])]
                (let [xs      (decode-numbers x)
                      control (reduce conj! control xs)]
                  (if (< (count xs) chunk-size) ; final frame
                    (persistent! control)
                    (recur (m/? ?read) control)))))))))))


(defn ^:deprecated message-writer
  "Returns a function taking an Electric message and returning a task writing it as individual frames using provided
   function. Might cut a message into chunks if its size would exceed the server payload limit.
   An empty message (0b) is written to notify the end of frame."
  [write]
  #(m/sp
     (loop [xs (seq (pop %))]
       (if-some [[x & xs] xs]
         (do (#?(:clj log/trace, :cljs js/console.debug) "🔼" x)
           (m/? (write
                  (try (encode x)
                    (catch #?(:clj Throwable :cljs :default) t
                      (throw (ex-info "Failed to encode" {:value x} t))))))
           (recur xs))
         (loop [xs (peek %)]
           (if (>= (count xs) chunk-size)
             (do (m/? (write (encode-numbers (subvec xs 0 chunk-size))))
               (recur (subvec xs chunk-size)))
             (m/? (write (encode-numbers xs)))))))))

(defn ^:deprecated decoder
  "A transducer partitioning a sequence of network messages into Electric events."
  [rf]
  (let [data (doto (object-array 2)
               (aset 0 []) (aset 1 []))]
    (fn
      ([] (rf))
      ([r]
       (assert (= [] (aget data 0) (aget data 1)))
       (rf r))
      ([r x]
       (if (string? x)
         (do (assert (= [] (aget data 1)))
             (aset data 0 (conj (aget data 0) (decode-str x))) r)
         (let [xs (decode-numbers x)]
           (aset data 1 (into (aget data 1) xs))
           (if (< (count xs) chunk-size)                  ; final frame
             (let [x (conj (aget data 0) (aget data 1))]
               (aset data 0 [])
               (aset data 1 [])
               (rf r x)) r)))))))

(defn ^:deprecated encoder
  "A transducer expanding Electric events to a sequence of network messages."
  [rf]
  (fn
    ([] (rf))
    ([r] (rf r))
    ([r x]
     (let [r (reduce rf r (eduction (map encode) (pop x)))
           r (reduce rf r (eduction (partition-all chunk-size) (map encode-numbers) (peek x)))]
       (case (mod (count (peek x)) chunk-size)
         0 (rf r (encode-numbers [])) r)))))

(defn foreach
  ([r] r)
  ([r x] (r x) r))
