(ns hyperfiddle.detest.muppet-flow
  (:require [contrib.data :refer [->box]]
            [contrib.debug :as dbg]
            [hyperfiddle.electric.impl.missionary-util :as mu]
            [missionary.core :as m])
  (:import [clojure.lang IFn IDeref]))

(defn muppet [rng]
  (let [!step (atom nil), !done (atom nil), !state (atom [false false false])]
    (fn
      ([rng] (let [<f> (->box nil)]
               (swap! !state
                 (fn [[stepped? done? canceled?]]
                   (<f> nil)
                   (let [f* (cond-> []
                              (and (not stepped?) (not done?)) (into (let [s [:step @!step]] [s s s s s s s s s]))
                              (and (not stepped?) (not done?)) (conj [:done @!done]))]
                     (if (seq f*)
                       (let [[t f] (nth f* (rng (count f*)))]
                         (<f> f)
                         (case t :step [true done? canceled?] :done [stepped? true canceled?]))
                       [stepped? done? canceled?]))))
               (<f>)))
      ([step done] (let [step (fn muppet-step [] (swap! !state assoc 0 true) (step))
                         done (fn muppet-done [] (swap! !state assoc 1 true) (done))]
                     (reset! !step step) (reset! !done done)
                     (step)                         ; todo, flows can be uninitialized
                     (reify
                       IFn (invoke [_] (swap! !state assoc 2 true))
                       IDeref (deref [_]
                                (swap! !state assoc 0 false)
                                (condp > (rng 10)
                                  1 (done)
                                  3 (step)
                                  #_else nil)
                                (let [n (rng 50)]
                                  (if (zero? n)
                                    (throw (ex-info "[muppet] intended crash" {::t :intended}))
                                    n)))))))))

(defn muppetize-root [flow done!]
  (let [!state (atom :transferred), ps (flow #(swap! !state (fn [state] (case state
                                                                          (:transferred) :stepped
                                                                          (:claimed) :stepped-during-transfer
                                                                          (:stepped :stepped-during-transfer) (throw (ex-info (str state " when stepping") {})))))
                                         done!)]
    ;; (add-watch !state :watch (fn [_ _ _v0 v1] (prn '===state=== v1)))
    (fn [rng]
      (let [<f> (->box nil)]
        (if (zero? (rng 20))
          (<f> (fn root-cancel [] (ps)))
          (swap! !state
            (fn [state]
              (<f> nil)
              (case state
                (:claimed :transferred :stepped-during-transfer) state
                (:stepped) (do (<f> (fn root-transfer []
                                      (let [v @ps]
                                        (swap! !state (fn [state] (case state
                                                                    (:claimed) :transferred
                                                                    (:stepped-during-transfer) :stepped
                                                                    (:stepped :transferred) (throw (ex-info (str state " when transfering") {})))))
                                        v)))
                               :claimed)))))
        (<f>)))))

(defn ->xorshift64 [seed]
  (let [!v (atom seed)]
    (fn step
      ([] (swap! !v (fn [v]
                      (let [v (bit-xor v (bit-shift-left v 7))]
                        (bit-xor v (unsigned-bit-shift-right v 9))))))
      ([n] (if (zero? n) n (mod (step) n))))))

(defn random-seed [] (-> java.security.SecureRandom new .nextLong))

(defn run-loop [muppet* rng done loop-name !trace]
  (try
    (loop [i 1]
      (Thread/yield)
      (if (< i 10000)
        (let [muppet (nth muppet* (rng (count muppet*)))]
          (when-some [f (muppet rng)] (swap! !trace conj [loop-name f]) (f))
          (when-not (m/? (m/race done (m/sp)))
            (recur (inc i))))
        (prn 'loop-consumed!!!)))
    (catch clojure.lang.ExceptionInfo e
      (when-not (#{:intended} (::t (ex-data e)))
        ;; (prn 'failed-trace @!trace)
        (throw e)))
    (catch Throwable e
      ;; (prn 'failed-trace @!trace)
      (throw e))))

(defn run-sync [root child* rng]
  (let [done (m/dfv)
        muppet* (conj child* (muppetize-root root #(done true)))]
    (run-loop muppet* rng done 0 (atom []))))

(defn fastest [& args]
  (m/absolve (apply m/race (map m/attempt args))))

(defn run-async [root child* rng]
  (let [done (m/dfv), !trace (atom [])
        muppet* (conj child* (muppetize-root root #(done true)))]
    (try (m/? (fastest
                done
                (m/via m/cpu (run-loop muppet* rng done 0 !trace))
                (m/via m/cpu (run-loop muppet* rng done 1 !trace))
                (m/sp (m/? (m/sleep 2000)) (throw (ex-info "failed to finish in 2s" {})))))
         (catch Throwable e
           (throw (ex-info "run-async failed" {:trace @!trace} e))))))

(comment
  (dotimes [i 100]
    (let [rng (->xorshift64 (random-seed))
          mup (muppet rng)
          !trace (atom [])
          child (mu/wrap-initialized :muppet mup {:reductions [(mu/log #(swap! !trace conj %))]})
          root (mu/wrap-initialized :root (m/latest identity child) {:reductions [(mu/log #(swap! !trace conj %))]})
          ;; child mup
          ;; root (m/latest identity child)
          ]
      (try (run-async root [mup] rng)
           (catch Throwable e
             (prn 'log-trace @!trace)
             (prn 'run-trace (-> e ex-data :trace))
             (throw e)))))
  )
