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

(defn flow
  ([input-flow] (flow "" input-flow))
  ([nm input-flow]
   (fn [step done]
     (let [!should-step? (atom ::init), !done? (atom false)
           step (fn []
                  (when @!done? (violated nm "step after done"))
                  (if (first (swap-vals! !should-step? not)) (cannot-throw nm step) (violated nm "double step")))
           done (fn [] (if (first (reset-vals! !done? true)) (violated nm "done called twice") (cannot-throw nm done)))
           cancel (try (input-flow step done)
                       (catch #?(:clj Throwable :cljs :default) e (violated nm "flow process creation threw" e)))]
       (when (= ::init @!should-step?) (violated nm "missing initial step"))
       (reify
         IFn (#?(:clj invoke :cljs -invoke) [_] (cannot-throw nm cancel))
         IDeref (#?(:clj deref :cljs -deref) [_]
                  (if-let [should-step (first (swap-vals! !should-step? not))]
                    (violated nm (if (= ::init should-step) "transfer without initial step" "double transfer"))
                    @cancel)))))))
