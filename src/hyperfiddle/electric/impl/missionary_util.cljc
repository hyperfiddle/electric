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
                    (swap! !es es/act {:type :step, :t (now-ms), :name nm})
                    (let [e (try (step) nil (catch #?(:clj Throwable :cljs :default) e e))]
                      (swap! !es es/act (cond-> {:type :stepped, :t (now-ms), :name nm} e (assoc :threw e)))
                      (when e (throw e))))
            done+ (fn []
                    (swap! !es es/act {:type :do, :t (now-ms), :name nm})
                    (let [e (try (done) nil (catch #?(:clj Throwable :cljs :default) e e))]
                      (swap! !es es/act (cond-> {:type :done, :t (now-ms), :name nm} e (assoc :threw e)))
                      (when e (throw e))))
            cancel (do
                     (swap! !es es/act {:type :init, :t (now-ms), :name nm})
                     (let [[t v] (try [:ok (flow step+ done+)] (catch #?(:clj Throwable :cljs :default) e [:ex e]))]
                       (swap! !es es/act (cond-> {:type :initialized, :t (now-ms), :name nm} (= :ex t) (assoc :threw v)))
                       (if (= :ex t) (throw v) v)))]
        (reify
          IFn (#?(:clj invoke :cljs -invoke) [_]
                (swap! !es es/act {:type :cancel, :t (now-ms), :name nm})
                (let [e (try (cancel) nil (catch #?(:clj Throwable :cljs :default) e e))]
                  (swap! !es es/act (cond-> {:type :canceled, :t (now-ms), :name nm} e (assoc :threw e)))
                  (when e (throw e))))
          IDeref (#?(:clj deref :cljs -deref) [_]
                   (swap! !es es/act {:type :transfer, :t (now-ms), :name nm})
                   (let [[t v] (try [:ok @cancel] (catch #?(:clj Throwable :cljs :default) e [:ex e]))]
                     (swap! !es es/act {:type :transferred, :t (now-ms), :name nm, :transfer-type t, :v v})
                     (if (= :ok t) v (throw v)))))))))

(defn log [log-fn] [:log nil #(log-fn %2)])

(def step-cannot-throw
  [:step-cannot-throw nil
   (fn [_ evt]
     (when (and (= :stepped (:type evt)) (:threw evt))
       (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] step cannot throw") {} (:threw evt)))))])

(def done-cannot-throw
  [:done-cannot-throw nil
   (fn [_ evt]
     (when (and (= :done (:type evt)) (:threw evt))
       (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] done cannot throw") {} (:threw evt)))))])

(def cancel-cannot-throw
  [:cancel-cannot-throw nil
   (fn [_ evt]
     (when (and (= :canceled (:type evt)) (:threw evt))
       (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] cancel cannot throw") {} (:threw evt)))))])

(def init-cannot-throw
  [:init-cannot-throw nil
   (fn [_ evt]
     (when (and (= :initialized (:type evt)) (:threw evt))
       (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] flow initialization cannot throw") {} (:threw evt)))))])

(def step-after-done
  [:step-after-done false
   (fn [done? evt]
     (if (= :done (:type evt))
       true
       (if (and (= :step (:type evt)) done?)
         (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] step after done") {}))
         done?)))])

(def step-after-throw
  [:step-after-throw nil
   (fn [threw? evt]
     (if (and (= :step (:type evt)) threw?)
       (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] step after throw") {}))
       (or threw? (:threw evt))))])

(def double-step
  [:double-step false
   (fn [stepped? evt]
     (cond (and stepped? (= :step (:type evt))) (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] double step") {}))
           (= :step (:type evt)) true
           (= :transfer (:type evt)) false
           :else stepped?))])

(def double-transfer
  [:double-transfer true
   (fn [transferred? evt]
     (cond (and transferred? (= :transfer (:type evt))) (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] double transfer") {}))
           (= :transfer (:type evt)) true
           (= :step (:type evt)) false
           :else transferred?))])

(def done-twice
  [:done-twice nil
   (fn [done? evt]
     (case (:type evt)
       (:do) (when done?
               (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] done called twice") {})))
       (:done) true
       #_else done?))])

(defn value-satisfies
  ([pred] (value-satisfies :value-satisfies pred))
  ([nm pred] [nm nil
              (fn [_ evt]
                (when (and (= :transferred (:type evt)) (= :ok (:transfer-type evt)) (not (pred (:v evt))))
                  (throw (ex-info (str "[" (:name evt) "]value " (pr-str (:v evt)) " doesn't satisfy predicate " pred " named " nm)
                           {:checker nm, :pred pred, :event evt}))))]))

(def initialized
  [:initialized nil
   (fn [stepped? evt]
     (case (:type evt)
       (:step) true
       (:initialized) (when-not stepped?
                        (throw (ex-info (str "[flow-protocol-violation][" (:name evt) "] flow marked as initialized but is not") {})))
       #_else stepped?))])

(def step-in-exceptional-transfer
  [:step-in-exceptional-transfer 0
   (fn [steps evt]
     (case (:type evt)
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
