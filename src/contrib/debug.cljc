(ns contrib.debug
  #?(:cljs (:require-macros contrib.debug))
  (:require [clojure.string :as str])
  (:import #?(:clj [clojure.lang IFn IDeref])
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
     (let [_ (dbgf [nm id 'spawned])
           it (flow #(do (dbgf [nm id 'notified]) (n)) #(do (dbgf [nm id 'terminated]) (t)))]
       (reify
         IFn (#?(:clj invoke :cljs -invoke) [_] (dbgf [nm id 'cancelled]) (it))
         IDeref (#?(:clj deref :cljs -deref) [_]
                  (let [v (try @it (catch #?(:clj Throwable :cljs :default) e [::ex e]))]
                    (dbgf [nm id 'transferred
                           (if false #_(instance? Failure v) ; FIXME Update to electric v3
                               (let [e (.-error v)]
                                 [(type e) (ex-message e)])
                               v)])
                    (if (and (vector? v) (= ::ex (first v)))
                      (throw (second v))
                      v))))))))
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
