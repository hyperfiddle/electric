(ns contrib.debug
  #?(:cljs (:require-macros contrib.debug))
  (:require [clojure.string :as str])
  (:import #?(:clj [clojure.lang IFn IDeref])
           [missionary Cancelled]
           #_[hyperfiddle.electric Failure] ; FIXME Update to electric v3
           ))

(def ^:dynamic *dbg* true)

(defmacro dbg
  ([form] `(dbg '~form ~form))
  ([label form]
   (let [[label form] (if (keyword? form) [form label] [label form])]
     `(if *dbg*
        (let [[st# v#] (try [:ok ~form] (catch ~(if (:js-globals &env) :default 'Throwable) ex# [:ex ex#]))]
          (prn ~label '~'==> v#)
          (if (= st# :ok) v# (throw v#)))
        ~form))))

(defmacro dbg-ok
  ([form] `(dbg-ok '~form ~form))
  ([label form]
   (let [[label form] (if (keyword? form) [form label] [label form])]
     `(let [v# ~form]
        (prn ~label '~'==> v#)
        v#))))

(defmacro dbg-when [form & body] `(binding [*dbg* ~form] ~@body))

(defmacro dbgv [form]
  `(if *dbg*
     (let [args# [~@form], v# ~form] (prn '~form '~'==> (cons '~(first form) (rest args#))  '~'==> v#) v#)
     ~form))

(defmacro dbgc [[op & args :as form]]
  `(let [op# ~op, args# ~args, ret# (apply op# args#)]
     (prn '~form)
     (doseq [[form# arg#] (map vector '~args args#)]
       (prn '~'_ form# '~'==> arg#))
     (prn '~'==> ret#)
     ret#))

(defmacro do-traced [& body] `(do ~@(for [form body] `(dbg ~form))))

(defn ->nprn [n]
  (let [prns (long-array [0])]
    (fn [& args]
      (when (< (aget prns (int 0)) n)
        (aset prns 0 (unchecked-inc (aget prns (int 0))))
        (apply prn args)))))

(def !id (atom 0))

(defn instrument*
  ([nm flow] (instrument* nm prn flow))
  ([nm dbgf flow] (instrument* nm (swap! !id inc) dbgf flow))
  ([nm id dbgf flow]
   (fn [n t]
     (let [_ (dbgf [nm id 'spawning])
           it (flow #(do (dbgf [nm id 'notifying]) (n) (dbgf [nm id 'notified]))
                #(do (dbgf [nm id 'terminating]) (t) (dbgf [nm id 'terminated])))
           proc (reify
                  IFn (#?(:clj invoke :cljs -invoke) [_] (dbgf [nm id 'cancelling]) (it) (dbgf [nm id 'cancelled]))
                  (#?(:clj invoke :cljs -invoke) [_ _] it)
                  IDeref (#?(:clj deref :cljs -deref) [_]
                           (dbgf [nm id 'transferring])
                           (let [[t v] (try [::ok @it] (catch #?(:clj Throwable :cljs :default) e [::ex e]))]
                             (dbgf [nm id 'transferred (if (= ::ex t) (if (instance? Cancelled v) 'Cancelled [(type v) (ex-message v) v]) v)])
                             (if (= ::ex t) (throw v) v))))]
       (dbgf [nm id 'spawned])
       proc))))
(defmacro instrument [nm v] `(hyperfiddle.electric3/input (instrument* ~nm (hyperfiddle.electric3/pure ~v))))

(defmacro js-measure [nm & body]
  (if (:js-globals &env)
    (let [st (str nm "-start"), fn (str nm "-end")]
      `(let [_# (js/performance.mark ~st)
             ret# (do ~@body)]
         (js/performance.mark ~fn)
         (js/performance.measure ~nm ~st ~fn)
         ret#))
    `(do ~@body)))

;; debugging options
;; [ ] turn off dynamically
;; [X] add label
;; [X] auto-label from expr
;; [ ] try/catch
;; [X] condition (debug when)
;; [X] print n times
;; [X] print view of value
;; [X] print meta
;; [X] custom printer
;; [X] opts map last (thread first support)

(defn ! [{label :label, catch? :catch, pred :when, n :times, view :view, meta? :meta?, printer :printer}]
  (let [!n (atom 0)]
    (fn [v]
      (let [v-as ((or view identity) v)]
        (when (and (or (nil? pred) (pred v)) (or (nil? n) (<= (swap! !n inc) n)))
          (binding [*print-meta* meta?]
            ((or printer prn) label '=> v-as)))
        v))))

(defmacro spy
  ([form] `(spy {:label '~form} ~form))
  ([opts form]
   (let [[opts form] (cond (map? opts) [opts form]
                           (map? form) [form opts]
                           :else       [opts form])
         opts (cond-> opts
                (not (contains? opts :label)) (assoc :label (list 'quote form)))]
     `((! ~opts) ~form))))

(comment
  ((! {:label 'hi}) #(+ 2 3))
  (spy 5)
  (spy (+ 2 3))
  (spy {} (+ 2 3))
  (spy {:when odd?} (+ 2 3))
  (spy {:when odd?} (+ 2 4))
  (spy {:label 'add} (+ (- 2 1) (- 3 1)))
  (spy {:view pr-str} (+ 2 3))
  (spy (if (odd? 1) 1 2))
  (def times2 (! {:times 2}))
  (times2 (+ 2 3))
  (spy {:meta? true} ^::hi `x)
  (spy {:printer #(fipp.edn/pprint %&)} (range 0 100))
  (spy 123 {:label 'last})
  )
