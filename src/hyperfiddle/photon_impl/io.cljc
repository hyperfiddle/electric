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
            [hyperfiddle.photon.debug :as dbg]
            #?(:cljs [com.cognitect.transit.types]))
  (:import (missionary Cancelled)
           (hyperfiddle.photon Failure Pending Remote)
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
        (let [err (.-error ^Failure x)]
          (cond (instance? Cancelled err) [:cancelled]
                (instance? Pending err)   [:pending]
                (instance? Remote err)    [:remote (dbg/serializable (ex-data err))]
                :else                     [:exception (ex-message err) (dbg/serializable (ex-data err))]))))}})

(def read-opts
  {:handlers
   {"failure"
    (t/read-handler
      (fn [[tag & args]]
        (case tag
          :exception (let [[message data] args]
                       (dbg/error (ex-info message data)))
          :remote    (let [[data] args]
                       (Failure. (ex-info "Remote error" data)))
          :pending   (Failure. (Pending.))
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

(defn encode
  "Encode a data frame to transit json"
  [x]
  (try
    #?(:clj (let [out (ByteArrayOutputStream.)]
              (t/write (t/writer out :json write-opts) x)
              (.toString out))
       :cljs (t/write (t/writer :json write-opts) x))
    (catch #?(:clj Throwable, :cljs :default) err
      ; 13:49:25.848 DEBUG h.p.io [qtp966786773-114] - Unserializable reference transfer:  datascript.db.TxReport@a1a5e94a
      ; {:value #datascript.db.TxReport{:db-before #datascript/DB {:schema {}, :datoms [[1 :task/description buy milk  ...
      (do (log/debug "Unserializable reference transfer: "
                     (str #_pr-str x)                       ; i.e. "datascript.db.TxReport@b532aead"
                     #_{:value x}                           ; don't ask logger to pr-str the entire datascript database
                     #_err)                                 ; don't spam log with scary error

          (if (instance? Failure x)
            (encode (Failure. (Remote.))) ; Failed to encode this exception, send a stub.
            (encode nil))))))

(defn decode
  "Decode a data frame from transit json"
  [^String s]
  #?(:clj (t/read (t/reader (ByteArrayInputStream. (.getBytes s "UTF-8")) :json read-opts))
     :cljs (t/read (t/reader :json read-opts) s)))

(defn decode-str [x]
  (try (doto (decode x) (->> (log/trace "🔽")))
    (catch #?(:clj Throwable :cljs :default) t
      (throw (ex-info "Failed to decode" {:value x} t)))))

; Jetty rejects websocket payloads larger than 65536 bytes by default
; We’ll chop messages if needed
(def chunk-size (bit-shift-right 65536 2))

(defn message-reader [?read]
  "Returns a discreet flow of read photon messages from provided task, emitting individual frames."
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


(defn message-writer
  "Returns a function taking a photon message and returning a task writing it as individual frames using provided
   function. Might cut a message into chunks if its size would exeeds the server payload limit. 
   An empty message (0b) is written to notify the end of frame."
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
         (loop [xs (peek %)]
           (if (>= (count xs) chunk-size)
             (do (m/? (write (encode-numbers (subvec xs 0 chunk-size))))
               (recur (subvec xs chunk-size)))
             (m/? (write (encode-numbers xs)))))))))
