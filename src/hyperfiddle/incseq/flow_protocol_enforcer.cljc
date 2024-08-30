(ns hyperfiddle.incseq.flow-protocol-enforcer
  (:require [hyperfiddle.electric.impl.array-fields :as a])
  #?(:clj (:import [clojure.lang IDeref IFn]))
  #?(:cljs (:require-macros [hyperfiddle.incseq.flow-protocol-enforcer :refer [cannot-throw]])))

(defn violated
  ([nm msg] (println nm "flow protocol violation:" msg) #?(:cljs (.error js/console) :clj (prn (Throwable.))))
  ([nm msg e]
   (println nm "flow protocol violation:" msg)
   (#?(:clj prn :cljs js/console.error) e)))

(defmacro cannot-throw [nm f] `(try (~f) (catch ~(if (:js-globals &env) :default 'Throwable) e#
                                        (violated ~nm ~(str f " cannot throw") e#))))

(def field-count (a/deffields -should-step -is-done))
(defn flow
  ([input-flow] (flow "" input-flow))
  ([nm input-flow]
   (fn [step done]
     (let [o (object-array field-count)
           _ (a/set o -should-step ::init, -is-done false)
           step (fn []
                  (when (a/get o -is-done) (violated nm "step after done"))
                  (if (a/getswap o -should-step not) (cannot-throw nm step) (violated nm "double step")))
           done (fn [] (if (a/getset o -is-done true) (violated nm "done called twice") (cannot-throw nm done)))
           cancel (try (input-flow step done)
                       (catch #?(:clj Throwable :cljs :default) e (violated "flow process creation threw" e)))]
       (reify
         IFn (#?(:clj invoke :cljs -invoke) [_] (cannot-throw nm cancel))
         IDeref (#?(:clj deref :cljs -deref) [_]
                  (if-let [should-step (a/getswap o -should-step not)]
                    (violated nm (if (= ::init should-step) "transfer without initial step" "double transfer"))
                    @cancel)))))))
