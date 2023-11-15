(ns hyperfiddle.electric.impl.expand-test
  #?(:clj
     (:require [cljs.env]
               [cljs.analyzer]
               [hyperfiddle.electric.impl.expand :as expand]
               [hyperfiddle.electric.impl.lang :as lang]
               [hyperfiddle.rcf :as rcf :refer [tests]]))
  #?(:cljs (:require-macros [hyperfiddle.electric.impl.expand-macro :as mac :refer [twice]])))

#?(:clj
   (defmacro all [o] `(expand/all ~(if (:js-globals &env)
                                     (assoc &env ::lang/peers {:client :cljs, :server :cljs}, ::lang/current :client)
                                     {:locals &env, ::lang/peers {:client :clj, :server :clj}, ::lang/current :client})
                        ~o)))

#?(:clj (defmacro test-peer-expansion [] (if (:js-globals &env) :cljs :clj)))

#?(:clj (deftype X []))

#?(:clj
   (tests
     (all nil) := nil
     (all 1) := 1
     (all '(inc 1)) := '(inc 1)
     (all '[(-> 1 inc)]) := '[(inc 1)]
     (all '{(-> 1 inc) (-> 1 inc)}) := '{(inc 1) (inc 1)}
     (all (seq '(-> 1 inc))) := '(inc 1)

     (all '(let [x 1] x)) := '(let* [x 1] x)
     (all '(let [x (let [y 1] y)] x)) := '(let* [x (let* [y 1] y)] x)

     (all '(do 1 2)) := '(do 1 2)
     (all '(do (let [x 1] x) (let [y 2] y))) := '(do (let* [x 1] x) (let* [y 2] y))

     (all '(inc (let [x 1] x))) := '(inc (let* [x 1] x))

     (all '(let [with-open inc] (with-open 1))) := '(let* [with-open inc] (with-open 1))
     (all '(let [with-open inc, x (with-open inc)] x)) := '(let* [with-open inc, x (with-open inc)] x)

     (all '(case (-> 1 inc) (2) (-> 2 inc) (with-open) 3 4)) := '(case (inc 1) (2) (inc 2) (with-open) 3 4)

     (all ''(-> 1 inc)) := ''(-> 1 inc)

     (all '(fn [x] 1)) := '(fn* ([x] 1))
     (all '(fn foo [x] 1)) := '(fn* foo ([x] 1))
     (all '(fn foo ([x] 1))) := '(fn* foo ([x] 1))
     (all '(fn [with-open] (with-open 1))) := '(fn* ([with-open] (with-open 1)))
     (all '(fn [x] (-> x inc))) := '(fn* ([x] (inc x)))

     (all '(fn* [x] x)) := '(fn* ([x] x)) ; fn* can come from elsewhere with a non-wrapped single arity

     (all '(letfn [(foo [with-open] (with-open 1)) ; don't expand with-open
                   (bar [x] (-> x inc))            ; expand ->
                   (baz [x] (->> x)) ; don't expand ->>, it is shadowed in letfn scope
                   (->> [x] x)]
             (-> (->> x) inc)))

     := '(letfn* [foo (fn* foo ([with-open] (with-open 1)))
                  bar (fn* bar ([x] (inc x)))
                  baz (fn* baz ([x] (->> x)))
                  ->> (fn* ->> ([x] x))]
           (inc (->> x)))

     (all '(set! (.-x (-> [(java.awt.Point. (-> 0 inc) 2)] first)) (-> 2 inc)))
     := '(set! (. (first [(new java.awt.Point (inc 0) 2)]) -x) (inc 2))

     (all '(new java.awt.Point (-> 0 inc) 1)) := '(new java.awt.Point (inc 0) 1)
     (all '(java.awt.Point. (-> 0 inc) 1)) := '(new java.awt.Point (inc 0) 1)
     (all '(new (missionary.core/seed [(-> 0 inc)]))) := '(new (missionary.core/seed [(inc 0)]))

     (all '(try (-> 1 inc)
                (catch Throwable with-open (with-open 1))
                (finally (-> 0 dec))))
     := '(try (inc 1)
              (catch Throwable with-open (with-open 1))
              (finally (dec 0)))

     (all '(catch (-> 1 inc))) := '(catch (inc 1))

     (all '(loop [with-open inc, x 2] (-> x with-open)))
     := '(loop* [with-open inc, x 2] (with-open x))

     (all '(binding [x (-> 1 inc)] (-> x inc))) := '(binding [x (inc 1)] (inc x))

     (all '((-> inc) 1)) := '(inc 1)

     (all '(try true)) := '(try true)
     ;; works outside RCF
     ;; (let [with-open inc] (all '(with-open 1)))
     ;; := '(with-open 1)

     (all '(hyperfiddle.impl.expand-test/X.)) := '(new hyperfiddle.impl.expand-test/X)

     (expand/-all '(#{:ok} 1) {:js-globals {}})

     "cljs var lookup doesn't produce undeclared-ns warnings"
     (let [!warns (atom [])]
       (cljs.env/ensure
         (cljs.analyzer/with-warning-handlers [(fn [typ env extra]
                                                 (when (typ cljs.analyzer/*cljs-warnings*)
                                                   (swap! !warns conj [typ env extra])))]
           (binding [*err* *out*]
             (with-out-str (expand/-all '(r/reflect 1) {::lang/peers {:client :cljs, :server :clj} ::lang/current :client})))))
       @!warns := [])

     "expansion is peer-aware"
     (expand/all {::lang/peers {:client :cljs, :server :clj}, ::lang/current :server}
       `[(test-peer-expansion) (::lang/toggle :client {} (test-peer-expansion))])
     := `[:clj (::lang/toggle :client {} :cljs)]

     (expand/all {::lang/peers {:client :cljs, :server :clj}, ::lang/current :client}
       `[(test-peer-expansion) (::lang/toggle :server {} (test-peer-expansion))])
     := `[:cljs (::lang/toggle :server {} :clj)]

     "cljs require-macros work in clj expansion"
     (expand/all {::lang/peers {:client :cljs, :server :clj}, ::lang/current :client, :ns 'hyperfiddle.electric.impl.expand-test}
       '(hyperfiddle.electric.impl.expand-macro/twice 1))
     := '[1 1]
     (expand/all {::lang/peers {:client :cljs, :server :clj}, ::lang/current :client, :ns 'hyperfiddle.electric.impl.expand-test}
       '(mac/twice 1))
     := '[1 1]
     (expand/all {::lang/peers {:client :cljs, :server :clj}, ::lang/current :client, :ns 'hyperfiddle.electric.impl.expand-test}
       '(twice 1))
     := '[1 1]

     (println " ok")))
