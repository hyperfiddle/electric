(ns hyperfiddle.electric.impl.missionary-util
  #?(:clj (:import [clojure.lang IDeref IFn]))
  (:require [contrib.debug :as dbg]
            [missionary.core :as m]
            [hyperfiddle.electric.impl.lang3 :as-alias lang]
            [hyperfiddle.electric.impl.dynamic-local :as dl]))

#?(:clj (set! *warn-on-reflection* true))

(defn now-ms []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (new js/Date))))

#?(:clj (defonce wrap (parse-boolean (System/getProperty "hyperfiddle.electric.impl.missionary-util.wrap" "false")))
   :cljs (goog-define wrap false))

(defonce wrap? (atom (fn [_nm _opts] wrap)))

(comment
  (reset! wrap? (fn [_ _] true))
  )

(defn act [^objects rd-array evt]
  (run! (fn [i] (aset rd-array (int i) ((aget rd-array (int (inc i))) (aget rd-array (int i)) evt)))
    (range 0 (count rd-array) 2))
  rd-array)

(def *mt (dl/local))

(defn wrap-flow [nm flow {:keys [reductions] :as opts}]
  (if-not (@wrap? nm opts)
    flow
    (let [mt (dl/get *mt)]
      (fn [step done]
        (let [id (random-uuid)
              rd-array (object-array (* 2 (count reductions)))
              _ (run! (fn [[i _nm init rf]]
                        (let [i (* i 2)]
                          (aset rd-array (int i) init)
                          (aset rd-array (int (+ i 1)) rf)))
                  (eduction (map-indexed cons) reductions))
              evt-base (assoc mt :name nm, :id id)
              !es (atom rd-array)
              step+ (fn []
                      (swap! !es act (assoc evt-base :event :step, :t (now-ms)))
                      (let [e (try (step) nil (catch #?(:clj Throwable :cljs :default) e e))]
                        (swap! !es act (cond-> (assoc evt-base :event :stepped, :t (now-ms)) e (assoc :threw e)))
                        (when e (throw e))))
              done+ (fn []
                      (swap! !es act (assoc evt-base :event :do, :t (now-ms)))
                      (let [e (try (done) nil (catch #?(:clj Throwable :cljs :default) e e))]
                        (swap! !es act (cond-> (assoc evt-base :event :done, :t (now-ms)) e (assoc :threw e)))
                        (when e (throw e))))
              cancel (do
                       (swap! !es act (assoc evt-base :event :spawn, :t (now-ms)))
                       (let [[t v] (try [:ok (flow step+ done+)] (catch #?(:clj Throwable :cljs :default) e [:ex e]))]
                         (swap! !es act (cond-> (assoc evt-base :event :spawned, :t (now-ms)) (= :ex t) (assoc :threw v)))
                         (cond-> v (= :ex t) throw)))]
          (reify
            IFn (#?(:clj invoke :cljs -invoke) [_]
                  (swap! !es act (assoc evt-base :event :cancel, :t (now-ms)))
                  (let [e (try (cancel) nil (catch #?(:clj Throwable :cljs :default) e e))]
                    (swap! !es act (cond-> (assoc evt-base :event :canceled, :t (now-ms)) e (assoc :threw e)))
                    (when e (throw e))))
            IDeref (#?(:clj deref :cljs -deref) [_]
                     (swap! !es act (assoc evt-base :event :transfer, :t (now-ms)))
                     (let [[t v] (try [:ok @cancel] (catch #?(:clj Throwable :cljs :default) e [:ex e]))]
                       (swap! !es act (assoc evt-base :event :transferred, :t (now-ms), :type t, :v v))
                       (cond-> v (= :ex t) throw)))))))))

(defn ->file-line-info [mt]
  (when (::lang/line mt)
    (str "L" (::lang/line mt) ":" (::lang/column mt)
      " " (::lang/ns mt) (when-some [d (::lang/def mt)] (str "/" d)))))

(defn ->violation-message [evt msg] (str "[flow-protocol-violation][" (:name evt) "][" (->file-line-info evt) "] " msg))

(defn log [log-fn] [:log nil #(log-fn %2)])

(def step-cannot-throw
  [:step-cannot-throw nil
   (fn [_ evt]
     (when (and (= :stepped (:event evt)) (:threw evt))
       (throw (ex-info (->violation-message evt "step cannot throw") {} (:threw evt)))))])

(def done-cannot-throw
  [:done-cannot-throw nil
   (fn [_ evt]
     (when (and (= :done (:event evt)) (:threw evt))
       (throw (ex-info (->violation-message evt "done cannot throw") {} (:threw evt)))))])

(def cancel-cannot-throw
  [:cancel-cannot-throw nil
   (fn [_ evt]
     (when (and (= :canceled (:event evt)) (:threw evt))
       (throw (ex-info (->violation-message evt "cancel cannot throw") {} (:threw evt)))))])

(def init-cannot-throw
  [:init-cannot-throw nil
   (fn [_ evt]
     (when (and (= :spawned (:event evt)) (:threw evt))
       (throw (ex-info (->violation-message evt "flow initialization cannot throw") {} (:threw evt)))))])

(def step-after-done
  [:step-after-done false
   (fn [done? evt]
     (if (= :done (:event evt))
       true
       (if (and (= :step (:event evt)) done?)
         (throw (ex-info (->violation-message evt "step after done") {}))
         done?)))])

(def step-after-throw
  [:step-after-throw nil
   (fn [threw? evt]
     (if (and (= :step (:event evt)) threw?)
       (throw (ex-info (->violation-message evt "step after throw") {}))
       (or threw? (:threw evt))))])

(def double-step
  [:double-step false
   (fn [stepped? evt]
     (cond (and stepped? (= :step (:event evt))) (throw (ex-info (->violation-message evt "double step") {}))
           (= :step (:event evt)) true
           (= :transfer (:event evt)) false
           :else stepped?))])

(def double-transfer
  [:double-transfer true
   (fn [transferred? evt]
     (cond (and transferred? (= :transfer (:event evt))) (throw (ex-info (->violation-message evt "double transfer") {}))
           (= :transfer (:event evt)) true
           (= :step (:event evt)) false
           :else transferred?))])

(def done-twice
  [:done-twice nil
   (fn [done? evt]
     (case (:event evt)
       (:do) (when done?
               (throw (ex-info (->violation-message evt "done called twice") {})))
       (:done) true
       #_else done?))])

(defn value-satisfies
  ([pred] (value-satisfies :value-satisfies pred))
  ([nm pred] [nm nil
              (fn [_ evt]
                (when (and (= :transferred (:event evt)) (= :ok (:type evt)) (not (pred (:v evt))))
                  (throw (ex-info (str "[" (:name evt) "][" (->file-line-info evt) "] value " (pr-str (:v evt)) " doesn't satisfy predicate " pred " named " nm)
                           {:checker nm, :pred pred, :event evt}))))]))

(def initialized
  [:spawned nil
   (fn [stepped? evt]
     (case (:event evt)
       (:step) true
       (:spawned) (when-not stepped?
                    (throw (ex-info (->violation-message evt "flow marked as initialized but is not") {})))
       #_else stepped?))])

(def step-in-exceptional-transfer
  [:step-in-exceptional-transfer 0
   (fn [steps evt]
     (case (:event evt)
       (:step) (inc steps)
       (:transfer) (dec steps)
       (:transferred) (if (and (pos? steps) (:threw evt))
                        (throw (ex-info (->violation-message evt "step after throw in transfer") {}))
                        steps)
       #_else steps))])

(defn flow-transfer-stalled [ms log]
  [:flow-transfer-stalled nil
   (let [sleeper (m/sleep ms)]
     (fn [cancel evt]
       (case (:event evt)
         (:step) ((m/sp (m/? sleeper) (log (str (some-> (->file-line-info evt) (str ": ")) "flow " (:name evt) " taking more than " ms "ms to transfer\n"))) {} {})
         (:transfer) (cancel)
         #_else cancel)))])

(defn flow-cancellation-stalled [ms log]
  [:flow-cancellation-stalled nil
   (fn [cancel evt]
     (case (:event evt)
       (:cancel) (or cancel ((m/sp (m/? (m/sleep ms)) (log (str (some-> (->file-line-info evt) (str ": ")) "flow " (:name evt) " taking more than " ms "ms to terminate after cancellation\n"))) {} {}))
       (:do) (do (when cancel (cancel)) #())
       #_else cancel))])

(defn shrink-grow? [{:keys [shrink grow permutation]}] (and (= 1 shrink grow) (= {0 1, 1 0} permutation)))
(defn shrink-grow-spotted [log]
  [:shrink-grow-spotted nil
   (fn [_ evt]
     (when (and (= :transferred (:event evt)) (= :ok (:type evt)) (shrink-grow? (:v evt)))
       (log (str "[" (:name evt) "][" (->file-line-info evt) "] shrink-grow diff spotted: " (pr-str (:v evt))))))])

(def -stall-ms 2000)

(def !stats (atom {:live 0, :done 0, :transfers 0}))

(def gather-stats
  [:gather-stats nil
   (fn [_ evt]
     (swap! !stats
       (fn [stats]
         (case (:event evt)
           (:spawned) (update stats :live inc)
           (:done) (-> stats (update :done inc) (update :live dec))
           (:transferred) (update stats :transfers inc)
           #_else stats))))])

#?(:cljs (defn format-stats [{:keys [live done transfers]}]
           (str "Live: " live "\nDone: " done "\nTransfers: " transfers)))

#?(:cljs
   (defn insert-stats! [!stats node]
     (m/observe
       (fn [!]
         (let [txt (.createTextNode js/document "")]
           (! nil)
           (.appendChild node txt)
           (.setInterval js/window #(set! (.-textContent txt) (format-stats @!stats)) 1000))))))

;; circular dependency, inline at usage site
;; (e/defn FlowStatsMonitor [] (e/client (dom/pre (e/input (insert-stats! !stats dom/node)))))

(def uninitialized-checks [#_(log prn) step-cannot-throw done-cannot-throw cancel-cannot-throw init-cannot-throw
                           step-after-done step-after-throw double-step double-transfer
                           done-twice step-in-exceptional-transfer #_gather-stats
                           #_(flow-transfer-stalled -stall-ms println) ; clogs the clock, revisit after optimizations
                           #_(shrink-grow-spotted println)               ; hundreds
                           (flow-cancellation-stalled -stall-ms println)]) ; experimental, not protocol violations
(def initialized-checks (conj uninitialized-checks initialized))
(def diff? (every-pred :grow :degree :shrink :change :permutation :freeze))
(def incseq-checks (conj initialized-checks (value-satisfies diff?)))

(defn wrap-uninitialized
  ([nm flow] (wrap-uninitialized nm flow {}))
  ([nm flow opts] (wrap-flow nm flow (update opts :reductions (fnil into []) uninitialized-checks))))

(defn wrap-initialized
  ([nm flow] (wrap-initialized nm flow {}))
  ([nm flow opts] (wrap-flow nm flow (update opts :reductions (fnil into []) initialized-checks))))

(defn wrap-incseq
  ([nm flow] (wrap-incseq nm flow {}))
  ([nm flow opts] (wrap-flow nm flow (update opts :reductions (fnil into []) incseq-checks))))


;;;;;;;;;;;
;; tasks ;;
;;;;;;;;;;;

;; cancel cannot throw

(defn wrap-task* [nm task {:keys [reductions] :as opts}]
  (if-not (@wrap? nm opts)
    task
    (fn [s f]
      (let [id (random-uuid)
            rd-array (object-array (* 2 (count reductions)))
            _ (run! (fn [[i _nm init rf]]
                      (let [i (* i 2)]
                        (aset rd-array (int i) init)
                        (aset rd-array (int (+ i 1)) rf)))
                (eduction (map-indexed cons) reductions))
            !es (atom rd-array)
            s+ (fn [v]
                 (swap! !es act {:event :success, :t (now-ms), :name nm, :id id, :v v})
                 (let [e (try (s v) nil (catch #?(:clj Throwable :cljs :default) e e))]
                   (swap! !es act (cond-> {:event :succeeded, :t (now-ms), :name nm, :id id, :v v} e (assoc :threw e)))
                   (when e (throw e))))
            f+ (fn [v]
                 (swap! !es act {:event :fail, :t (now-ms), :name nm, :id id, :v v})
                 (let [e (try (f v) nil (catch #?(:clj Throwable :cljs :default) e e))]
                   (swap! !es act (cond-> {:event :failed, :t (now-ms), :name nm, :id id, :v v} e (assoc :threw e)))
                   (when e (throw e))))]
        (swap! !es act {:event :spawn, :t (now-ms), :name nm, :id id})
        (let [[t cancel] (try [:ok (task s+ f+)] (catch #?(:clj Throwable :cljs :default) e [:ex e]))]
          (swap! !es act (cond-> {:event :spawned, :t (now-ms), :name nm, :id id} (= :ex t) (assoc :threw cancel)))
          (cond-> cancel (= :ex t) throw))))))

(def spawn-cannot-throw
  [:spawn-cannot-throw nil
   (fn [_ evt]
     (when (and (= :spawned (:event evt)) (:threw evt))
       (throw (ex-info (str "[task-protocol-violation][" (:name evt) "] task spawn cannot throw") {} (:threw evt)))))])

(def success-cannot-throw
  [:success-cannot-throw nil
   (fn [_ evt]
     (when (and (= :succeeded (:event evt)) (:threw evt))
       (throw (ex-info (str "[task-protocol-violation][" (:name evt) "] task success cannot throw") {} (:threw evt)))))])

(def failure-cannot-throw
  [:failure-cannot-throw nil
   (fn [_ evt]
     (when (and (= :failed (:event evt)) (:threw evt))
       (throw (ex-info (str "[task-protocol-violation][" (:name evt) "] task failure cannot throw") {} (:threw evt)))))])

(def task-cancel-cannot-throw
  [:cancel-cannot-throw nil
   (fn [_ evt]
     (when (and (= :canceled (:event evt)) (:threw evt))
       (throw (ex-info (str "[task-protocol-violation][" (:name evt) "] cancel cannot throw") {} (:threw evt)))))])

(def single-shot-continuation
  [:single-shot-continuation false
   (fn [called? evt]
     (case (:event evt)
       (:success :fail) (if called?
                          (throw (ex-info (str "[task-protocol-violation][" (:name evt) "] task can only complete once") {}))
                          true)
       #_else called?))])

(defn task-stalled [ms log]
  [:task-stalled nil
   (fn [cancel evt]
     (case (:event evt)
       (:spawn) ((m/sp (m/? (m/sleep ms)) (log (str "task " (:name evt) " taking more than " ms "ms to complete"))) {} {})
       (:success :fail) (cancel)
       #_else cancel))])

(defn task-cancellation-stalled [ms log]
  [:task-cancellation-stalled nil
   (fn [cancel evt]
     (case (:event evt)
       (:cancel) (or cancel ((m/sp (m/? (m/sleep ms)) (log (str "task " (:name evt) " taking more than " ms "ms to cancel"))) {} {}))
       (:success :fail) (if cancel (cancel) #())
       #_else cancel))])

(def task-checks [spawn-cannot-throw success-cannot-throw failure-cannot-throw task-cancel-cannot-throw single-shot-continuation
                  (task-stalled -stall-ms println) (task-cancellation-stalled -stall-ms println)]) ; experimental

(defn wrap-task
  ([nm task] (wrap-task nm task {}))
  ([nm task opts] (wrap-task* nm task (update opts :reductions (fnil into []) task-checks))))

(comment
  (m/? (wrap-task* 'foo (m/sleep 1000 :ok) {:reductions [(task-stalled 500 println)]}))
  )
