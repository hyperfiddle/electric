(ns hyperfiddle.electric.impl.missionary-util
  #?(:clj (:import [clojure.lang IDeref IFn]))
  (:require [contrib.debug :as dbg]
            [hyperfiddle.electric.impl.event-store :as es]))

(defn now-ms []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (new js/Date))))

#?(:clj (defonce wrap (parse-boolean (System/getProperty "hyperfiddle.electric.impl.missionary-util.wrap" "false")))
   :cljs (goog-define wrap false))

(defonce wrap? (atom (fn [_nm _opts] wrap)))

(defn wrap-flow [nm flow {:keys [projections] :as opts}]
  (if-not (@wrap? nm opts)
    flow
    (fn [step done]
      (let [!es (atom (reduce (fn [es p] (apply es/project es p)) es/empty projections))
            step+ (fn []
                    (swap! !es es/act {:event :step, :t (now-ms), :name nm})
                    (let [e (try (step) nil (catch #?(:clj Throwable :cljs :default) e e))]
                      (swap! !es es/act (cond-> {:event :stepped, :t (now-ms), :name nm} e (assoc :threw e)))
                      (when e (throw e))))
            done+ (fn []
                    (swap! !es es/act {:event :do, :t (now-ms), :name nm})
                    (let [e (try (done) nil (catch #?(:clj Throwable :cljs :default) e e))]
                      (swap! !es es/act (cond-> {:event :done, :t (now-ms), :name nm} e (assoc :threw e)))
                      (when e (throw e))))
            cancel (do
                     (swap! !es es/act {:event :spawn, :t (now-ms), :name nm})
                     (let [[t v] (try [:ok (flow step+ done+)] (catch #?(:clj Throwable :cljs :default) e [:ex e]))]
                       (swap! !es es/act (cond-> {:event :spawned, :t (now-ms), :name nm} (= :ex t) (assoc :threw v)))
                       (cond-> v (= :ex t) throw)))]
        (reify
          IFn (#?(:clj invoke :cljs -invoke) [_]
                (swap! !es es/act {:event :cancel, :t (now-ms), :name nm})
                (let [e (try (cancel) nil (catch #?(:clj Throwable :cljs :default) e e))]
                  (swap! !es es/act (cond-> {:event :canceled, :t (now-ms), :name nm} e (assoc :threw e)))
                  (when e (throw e))))
          IDeref (#?(:clj deref :cljs -deref) [_]
                   (swap! !es es/act {:event :transfer, :t (now-ms), :name nm})
                   (let [[t v] (try [:ok @cancel] (catch #?(:clj Throwable :cljs :default) e [:ex e]))]
                     (swap! !es es/act {:event :transferred, :t (now-ms), :name nm, :type t, :v v})
                     (cond-> v (= :ex t) throw))))))))

(defn log [log-fn] [:log nil #(log-fn %2)])

(def step-cannot-throw
  [:step-cannot-throw nil
   (fn [_ evt]
     (when (and (= :stepped (:event evt)) (:threw evt))
       (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] step cannot throw") {} (:threw evt)))))])

(def done-cannot-throw
  [:done-cannot-throw nil
   (fn [_ evt]
     (when (and (= :done (:event evt)) (:threw evt))
       (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] done cannot throw") {} (:threw evt)))))])

(def cancel-cannot-throw
  [:cancel-cannot-throw nil
   (fn [_ evt]
     (when (and (= :canceled (:event evt)) (:threw evt))
       (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] cancel cannot throw") {} (:threw evt)))))])

(def init-cannot-throw
  [:init-cannot-throw nil
   (fn [_ evt]
     (when (and (= :spawned (:event evt)) (:threw evt))
       (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] flow initialization cannot throw") {} (:threw evt)))))])

(def step-after-done
  [:step-after-done false
   (fn [done? evt]
     (if (= :done (:event evt))
       true
       (if (and (= :step (:event evt)) done?)
         (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] step after done") {}))
         done?)))])

(def step-after-throw
  [:step-after-throw nil
   (fn [threw? evt]
     (if (and (= :step (:event evt)) threw?)
       (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] step after throw") {}))
       (or threw? (:threw evt))))])

(def double-step
  [:double-step false
   (fn [stepped? evt]
     (cond (and stepped? (= :step (:event evt))) (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] double step") {}))
           (= :step (:event evt)) true
           (= :transfer (:event evt)) false
           :else stepped?))])

(def double-transfer
  [:double-transfer true
   (fn [transferred? evt]
     (cond (and transferred? (= :transfer (:event evt))) (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] double transfer") {}))
           (= :transfer (:event evt)) true
           (= :step (:event evt)) false
           :else transferred?))])

(def done-twice
  [:done-twice nil
   (fn [done? evt]
     (case (:event evt)
       (:do) (when done?
               (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] done called twice") {})))
       (:done) true
       #_else done?))])

(defn value-satisfies
  ([pred] (value-satisfies :value-satisfies pred))
  ([nm pred] [nm nil
              (fn [_ evt]
                (when (and (= :transferred (:event evt)) (= :ok (:type evt)) (not (pred (:v evt))))
                  (throw (ex-info (str "[" (:name evt) "]value " (pr-str (:v evt)) " doesn't satisfy predicate " pred " named " nm)
                           {:checker nm, :pred pred, :event evt}))))]))

(def initialized
  [:spawned nil
   (fn [stepped? evt]
     (case (:event evt)
       (:step) true
       (:spawned) (when-not stepped?
                        (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] flow marked as initialized but is not") {})))
       #_else stepped?))])

(def step-in-exceptional-transfer
  [:step-in-exceptional-transfer 0
   (fn [steps evt]
     (case (:event evt)
       (:step) (inc steps)
       (:transfer) (dec steps)
       (:transferred) (if (and (pos? steps) (:threw evt))
                        (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] step after throw in transfer") {}))
                        steps)
       #_else steps))])

(def uninitialized-checks [step-cannot-throw done-cannot-throw cancel-cannot-throw init-cannot-throw
                           step-after-done step-after-throw double-step double-transfer
                           done-twice step-in-exceptional-transfer])
(def initialized-checks (conj uninitialized-checks initialized))
(def diff? (every-pred :grow :degree :shrink :change :permutation :freeze))
(def incseq-checks (conj initialized-checks (value-satisfies diff?)))

(defn wrap-uninitialized
  ([nm flow] (wrap-uninitialized nm flow {}))
  ([nm flow opts] (wrap-flow nm flow (update opts :projections (fnil into []) uninitialized-checks))))

(defn wrap-incseq
  ([nm flow] (wrap-incseq nm flow {}))
  ([nm flow opts] (wrap-flow nm flow (update opts :projections (fnil into []) incseq-checks))))
