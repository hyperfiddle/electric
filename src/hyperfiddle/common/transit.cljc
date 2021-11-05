(ns hyperfiddle.common.transit
  (:require [cognitect.transit :as t]
            [fipp.ednize :as ednize]
            [hyperfiddle.api :as hf :refer #?(:clj [] :cljs [Link Input])]
            [hfdl.impl.runtime :as pr :refer #?(:clj [] :cljs [Failure Pending])]
            #?(:cljs [com.cognitect.transit.types]))
  #?(:clj (:import (hyperfiddle.api Link Input)
                   (hfdl.impl.runtime Failure Pending)
                   (java.io ByteArrayInputStream ByteArrayOutputStream)
                   (clojure.lang ExceptionInfo))))

(def read-handlers
  (atom {"ex-info"               (t/read-handler #(apply ex-info %))
         "hyperfiddle.api.Link"  (t/read-handler #(apply hf/->Link %))
         "hyperfiddle.api.Input" (t/read-handler (fn [[id value]] (hf/->Input id value nil)))
         "hyperfiddle.Failure"   (t/read-handler #(apply pr/->Failure %))
         "hyperfiddle.Pending"   (t/read-handler #(apply pr/->Pending %))
         "missionary.Cancelled"  (t/read-handler (fn [_] (missionary.Cancelled.)))}))

(def write-handlers
  (atom {ExceptionInfo (t/write-handler (constantly "ex-info") (fn [ex] [(ex-message ex) (ex-data ex) (ex-cause ex)]))
         Link          (t/write-handler (constantly "hyperfiddle.api.Link") (fn [^Link x] [(.-href x) (.-value x)]))
         Input         (t/write-handler (constantly "hyperfiddle.api.Input") (fn [^Input x] [(.-id x) (.-value x)]))
         Failure       (t/write-handler (constantly "hyperfiddle.Failure") (juxt :error))
         Pending       (t/write-handler (constantly "hyperfiddle.Pending") (constantly []))
         missionary.Cancelled (t/write-handler (constantly "missionary.Cancelled") (constantly []))}))

(def ^:dynamic string-encoding "UTF-8")

(defn with-refresh
  "Recompute and remember the return value of `buildf` if `get-new-valuef` return
  a different value than the previous call."
  [get-new-valuef buildf]
  (let [state (volatile! nil)] ; don't need STM
    (fn []
      (if-not (= @state (get-new-valuef))
        (vreset! state (buildf))
        @state))))

;; Hot module loading might swap! new handlers in. ^:dynamic can't help here.
(def ^:private default-reader (with-refresh #(deref read-handlers)  #(t/reader :json {:handlers @read-handlers})))
(def ^:private default-writer (with-refresh #(deref write-handlers) #(t/writer :json {:handlers @write-handlers})))

;; Custom handlers are allowed as decode and encode are not just for network
(defn decode
  "Transit decode an object from `s`."
  [s & {:keys [type opts]}]
  #?(:clj  (let [type (or type :json)
                 opts (or opts {:handlers @read-handlers})
                 in   (ByteArrayInputStream. (.getBytes s string-encoding))
                 rdr  (t/reader in type opts)]
             (t/read rdr))
     :cljs (let [rdr (if (or type opts)
                       (t/reader type (update opts :handlers merge read-handlers))
                       (default-reader))]
             (t/read rdr s))))

(defn encode
  "Transit encode `x` into a String."
  [x & {:keys [type opts]}]
  #?(:clj  (let [type   (or type :json)
                 opts   (or opts {:handlers @write-handlers})
                 out    (ByteArrayOutputStream.)
                 writer (t/writer out type opts)]
             (t/write writer x)
             (.toString out))
     :cljs (let [wrtr (if (or type opts)
                        (t/writer type (update opts :handlers merge write-handlers))
                        (default-writer))]
             (t/write wrtr x))))

;;;;;;;;;;;;;;;;
;; EXTENTIONS ;;
;;;;;;;;;;;;;;;;

#?(:cljs (extend-type com.cognitect.transit.types/UUID IUUID)) ; https://github.com/hyperfiddle/hyperfiddle/issues/728

(extend-protocol ednize/IEdn
  Link
  (-edn [this]
    (tagged-literal 'hyperfiddle.api.Link {:href  #?(:clj (.href this)  :cljs (.-href this))
                                           :value #?(:clj (.value this) :cljs (.-value this))}))
  Input
  (-edn [this]
    (tagged-literal 'hyperfiddle.api.Input {:id    #?(:clj (.id this) :cljs (.-id this))
                                            :value #?(:clj (.value this) :cljs (.-value this))})))

#?(:cljs (extend-protocol ednize/IEdn
           com.cognitect.transit.types/UUID
           (-edn [this]
             (tagged-literal 'uuid (.toString this)))))
