(ns hyperfiddle.incseq.flow-protocol-enforcer
  #?(:clj (:import [clojure.lang IDeref IFn]))
  #?(:cljs (:require-macros [hyperfiddle.incseq.flow-protocol-enforcer :refer [cannot-throw]])))

(defn violated
  ([nm msg] (println nm "flow protocol violation:" msg) #?(:cljs (.error js/console) :clj (prn (Throwable.))))
  ([nm msg e]
   (println nm "flow protocol violation:" msg)
   (#?(:clj prn :cljs js/console.error) e)))

(defmacro cannot-throw [nm f] `(try (~f) (catch ~(if (:js-globals &env) :default 'Throwable) e#
                                        (violated ~nm ~(str f " cannot throw") e#))))

(def diff? (every-pred :grow :degree :shrink :change :permutation :freeze))

(defn is-array? [v] #?(:clj (-> v class .isArray) :cljs (array? v)))
(defn pretty [v] (cond-> v (is-array? v) vec))

(defn enforce
  ([input-flow] (enforce {} input-flow))
  ([{nm :name, f :transfer, :as o} input-flow]
   (fn [step done]
     (let [!should-step? (atom ::init), !done? (atom false), !v (atom ::init)
           step (fn []
                  (when @!done? (violated nm "step after done"))
                  (if (first (swap-vals! !should-step? not)) (cannot-throw nm step) (violated nm "double step")))
           done (fn [] (if (first (reset-vals! !done? true)) (violated nm "done called twice") (cannot-throw nm done)))
           cancel (try (input-flow step done)
                       (catch #?(:clj Throwable :cljs :default) e (violated nm "flow process creation threw" e)))
           check-transfer (if f
                            (fn [t v]
                              (when (and (= :ok t) (not (f v)))
                                (violated nm (str "transferred value doesn't satisfy " f ": " (pretty v)))))
                            (fn [_ _]))]
       (when (and (:initialized o) (= ::init @!should-step?)) (violated nm "missing initial step"))
       (reify
         IFn (#?(:clj invoke :cljs -invoke) [_] (cannot-throw nm cancel))
         IDeref (#?(:clj deref :cljs -deref) [_]
                  (if-let [should-step (first (swap-vals! !should-step? not))]
                    (let [[t v] (try [:ok (reset! !v @cancel)] (catch #?(:clj Throwable :cljs :default) e [:ex (reset! !v e)]))]
                      (violated nm (if (= ::init should-step) "transfer without initial step" "double transfer"))
                      (when (= :ex t) (#?(:clj prn :cljs js/console.error) v))
                      (check-transfer t v)
                      (if (= :ex t) (throw v) v))
                    (let [[t v] (try [:ok (reset! !v @cancel)] (catch #?(:clj Throwable :cljs :default) e [:ex (reset! !v e)]))]
                      (check-transfer t v)
                      (if (= :ex t) (throw v) v)))))))))

(defn incseq [nm flow] (enforce {:name nm, :initialized true, :transfer #'diff?} flow))
(defn initialized [nm flow] (enforce {:name nm, :initialized true} flow))
(defn uninitialized [nm flow] (enforce {:name nm} flow))
