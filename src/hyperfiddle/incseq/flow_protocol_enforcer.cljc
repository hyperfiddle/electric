(ns hyperfiddle.incseq.flow-protocol-enforcer
  #?(:clj (:import [clojure.lang IDeref IFn]))
  #?(:cljs (:require-macros [hyperfiddle.incseq.flow-protocol-enforcer :refer [cannot-throw]])))

(defn %violated
  ([nm msg] (println nm "flow protocol violation:" msg) #?(:cljs (.error js/console) :clj (prn (Throwable.))))
  ([nm msg e]
   (println nm "flow protocol violation:" msg)
   (#?(:clj prn :cljs js/console.error) e)))

(defmacro cannot-throw [f nm violated]
  `(try (~f) (catch ~(if (:js-globals &env) :default 'Throwable) e#
               (~violated ~nm ~(str f " cannot throw") e#))))

(def diff? (every-pred :grow :degree :shrink :change :permutation :freeze))

(defn is-array? [v] #?(:clj (-> v class .isArray) :cljs (array? v)))
(defn pretty [v] (cond-> v (is-array? v) vec))

(defn enforce
  ([flow] (enforce {} flow))
  ([{nm :name, f :transfer, violated :on-violate :as o :or {violated %violated}} flow]
   (fn [step done]
     (let [!should-step? (atom ::init), !done? (atom false)
           step (fn []
                  (when @!done? (violated nm "step after done"))
                  (if (first (swap-vals! !should-step? not)) (cannot-throw step nm violated) (violated nm "double step")))
           done (fn [] (if (first (reset-vals! !done? true)) (violated nm "done called twice") (cannot-throw done nm violated)))
           cancel (try (flow step done)
                       (catch #?(:clj Throwable :cljs :default) e (violated nm "flow process creation threw" e)))
           check-transfer (if f
                            (fn [t v]
                              (when (and (= :ok t) (not (f v)))
                                (violated nm (str "transferred value doesn't satisfy " f ": " (pretty v)))))
                            (fn [_ _]))]
       (when (and (:initialized o) (= ::init @!should-step?)) (violated nm "missing initial step"))
       (reify
         IFn (#?(:clj invoke :cljs -invoke) [_] (cannot-throw cancel nm violated))
         IDeref (#?(:clj deref :cljs -deref) [_]
                  (let [should-step (first (swap-vals! !should-step? not))
                        [t v] (try [:ok @cancel] (catch #?(:clj Throwable :cljs :default) e [:ex e]))]
                    (check-transfer t v)
                    (when should-step (violated nm "transfer without step" (when (= :ex t) v)))
                    (if (= :ex t) (throw v) v))))))))

(defn incseq [nm flow] (enforce {:name nm, :initialized true, :transfer #'diff?} flow))
(defn initialized [nm flow] (enforce {:name nm, :initialized true} flow))
(defn uninitialized [nm flow] (enforce {:name nm} flow))
