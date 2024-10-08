(ns hyperfiddle.detest
  (:import #?(:clj [clojure.lang IFn IDeref])
           #?(:clj [clojure.lang ExceptionInfo])
           #?(:cljs [goog.math Long])
           [missionary Cancelled])
  (:require [hyperfiddle.incseq.flow-protocol-enforcer :as fpe]
            #?(:cljs [contrib.data :refer [->box]])
            [contrib.debug :as dbg]
            [clojure.string :as str]))

;; DETErministic TESTing, so naturally, DETEST

#?(:clj
   (defn ->xorshift64 [seed]
     (let [!v (atom seed)]
       (fn step
         ([] (swap! !v (fn [v]
                         (let [v (bit-xor v (bit-shift-left v 7))]
                           (bit-xor v (unsigned-bit-shift-right v 9))))))
         ([n] (if (zero? n) n (mod (step) n)))))))

#?(:clj (defn random-seed [] (-> java.security.SecureRandom new .nextLong)))

#?(:cljs
   (defn ->xorshift64 [seed]
     (let [<v> (->box seed)
           step (fn [] (let [^Long v (<v>)
                             ^Long v (.xor v (.shiftLeft v 7))]
                         (<v> (.xor v (.shiftRightUnsigned v 9)))))]
       (fn self
         ([] (let [^Long v (step)] (.toNumber v)))
         ([n] (if (zero? n) n (let [^Long v (step)] (mod (.toNumber v) n))))))))

#?(:cljs (defn random-seed [] (Long/fromBits (rand-int 0x100000000) (rand-int 0x100000000))))

(defprotocol Engine
  (exercise [this flow])
  (->rng [this])
  (->opts [this])
  (->dbgf [this])
  (roll [this] [this nm])
  (add-proc [this proc])
  (del-proc [this proc]))

(defn instrument [nm ngn flow]
  (fn [step done]
    (let [it ((dbg/instrument* nm (->dbgf ngn) flow) step done)]
      (reify
        IFn
        (#?(:clj invoke :cljs -invoke) [_] (it))
        (#?(:clj invoke :cljs -invoke) [_ n] ((it :process) n))
        IDeref
        (#?(:clj deref :cljs -deref) [_] @it)))))

(defn on-violate
  ([nm msg] (on-violate nm msg nil))
  ([nm msg e] (throw (ex-info (str nm " flow protocol violation: " msg) {} e))))

(defn debug? [ngn] (-> ngn ->opts :debug))

(defn ->engine
  ([] (->engine {}))
  ([{:keys [seed] :as o}]
   (let [seed (or seed (random-seed)), rng (->xorshift64 seed), !proc* (atom [])
         !n (atom 0), dbgf (case (:debug o)
                             (:steps) (fn [_] (swap! !n inc))
                             (:full) (fn [x] (swap! !n inc) (prn x))
                             #_else prn)]
     (reify Engine
       (add-proc [_ proc] (swap! !proc* conj proc))
       (del-proc [_ proc] (swap! !proc* (fn [proc*] (filterv #(not= % proc) proc*))))
       (roll [_] (rng))
       (roll [_ n] (rng n))
       (->rng [_] rng)
       (->opts [_] o)
       (->dbgf [_] dbgf)
       (exercise [this flow]
         (try (let [flow (fpe/enforce {:name 'root, :on-violate on-violate}
                           (cond->> flow (debug? this) (dbg/instrument* 'root dbgf)))
                    !s (atom nil)
                    root (flow #(reset! !s :step) #(reset! !s :done))]
                (add-proc this root)
                (while (not= :done @!s)
                  (let [proc* @!proc*, n (rng (count proc*)), proc (nth proc* n)]
                    (if (= proc root)
                      (case @!s
                        (:done nil) (when (> 1 (rng 100)) (root))
                        (:step) (condp > (rng 100)
                                  1 (root)
                                  25 nil
                                  #_else (try (reset! !s nil) @root
                                              (catch Cancelled _)
                                              (catch ExceptionInfo e
                                                (when-not (str/starts-with? (ex-message e) "[DETEST OK] ")
                                                  (throw e))))))
                      (proc (rng)))))
                (dotimes [_ (rng 10)] (root)))
              (catch #?(:clj Throwable :cljs :default) e
                (throw (ex-info (str "exercise failed") {:seed seed, :steps @!n} e)))))))))

(defn minimize [ngn <s> flow]
  (try (exercise ngn flow)
       (catch ExceptionInfo e
         (let [[n0] (<s>), n1 (-> e ex-data :steps)]
           (when (or (nil? n0) (< n1 n0)) (<s> [n1 (-> e ex-data :seed)]))))))
