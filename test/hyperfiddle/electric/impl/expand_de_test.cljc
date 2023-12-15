(ns hyperfiddle.electric.impl.expand-de-test
  (:require #?(:clj [cljs.env])
            #?(:clj [cljs.analyzer])
            #?(:clj [hyperfiddle.electric.impl.compiler :as c])
            #?(:clj [hyperfiddle.electric.impl.runtime-de :as r])
            #?(:clj [hyperfiddle.electric :as-alias e])
            [hyperfiddle.electric.impl.expand-require-referred :as ref :refer [referred]]
            #?(:clj [hyperfiddle.rcf :as rcf :refer [tests]]))
  #?(:cljs (:require-macros [hyperfiddle.electric.impl.expand-macro :as mac :refer [twice]])))

#?(:clj
   (defmacro all [o] `(c/expand-all ~(if (:js-globals &env)
                                       (assoc &env ::c/peers {:client :cljs, :server :cljs}, ::c/current :client)
                                       {:locals &env, ::c/peers {:client :clj, :server :clj}, ::c/current :client})
                        ~o)))

#?(:clj (defmacro test-peer-expansion [] (if (:js-globals &env) :cljs :clj)))

#?(:clj (deftype X []))

#?(:clj (def has-line-meta? (comp number? :line meta)))

#?(:clj
   (tests
     (all nil) := nil
     (all 1) := 1
     (all '(inc 1)) := '(inc 1)
     (has-line-meta? (all '(inc 1))) := true
     (all '[(-> 1 inc)]) := '[(inc 1)]
     (has-line-meta? (-> (all '[(-> 1 inc)]) first)) := true
     (all '{(-> 1 inc) (-> 1 inc)}) := '{(inc 1) (inc 1)}
     (all (seq '(-> 1 inc))) := '(inc 1)

     (all '(let [x 1] x)) := '(let* [x 1] x)
     (has-line-meta? (all '(let [x 1] x))) := true
     (all '(let [x (let [y 1] y)] x)) := '(let* [x (let* [y 1] y)] x)

     (all '(do 1 2)) := (all `(e/amb (e/drain 1) 2))
     (has-line-meta? (all '(do 1 2))) := true
     (all '(do (let [x 1] x) (let [y 2] y))) := (all `(e/amb (e/drain (let* [~'x 1] ~'x)) (let* [~'y 2] ~'y)))
     (all '(do (-> 1 inc))) := '(inc 1)
     (has-line-meta? (all '(do (-> 1 inc)))) := true

     (all '(inc (let [x 1] x))) := '(inc (let* [x 1] x))

     (all '(let [with-open inc] (with-open 1))) := '(let* [with-open inc] (with-open 1))
     (all '(let [with-open inc, x (with-open inc)] x)) := '(let* [with-open inc, x (with-open inc)] x)

     (all '(case (-> 1 inc) (2) (-> 2 inc) (with-open) 3 4)) := '(case (inc 1) (2) (inc 2) (with-open) 3 4)
     (has-line-meta? (all '(case (-> 1 inc) (2) (-> 2 inc) (with-open) 3 4))) := true

     (all ''(-> 1 inc)) := ''(-> 1 inc)

     (all '(fn [x] 1)) := '(fn* ([x] 1))
     (has-line-meta? (all '(fn [x] 1))) := true
     (all '(fn foo [x] 1)) := '(fn* foo ([x] 1))
     (all '(fn foo ([x] 1))) := '(fn* foo ([x] 1))
     (all '(fn [with-open] (with-open 1))) := '(fn* ([with-open] (with-open 1)))
     (all '(fn [x] (-> x inc))) := '(fn* ([x] (inc x)))

     (all '(fn* [x] x)) := '(fn* ([x] x)) ; fn* can come from elsewhere with a non-wrapped single arity
     (has-line-meta? (all '(fn* [x] x))) := true

     (let [x (all '(letfn [(foo [with-open] (with-open 1)) ; don't expand with-open
                           (bar [x] (-> x inc))            ; expand ->
                           (baz [x] (->> x)) ; don't expand ->>, it is shadowed in letfn scope
                           (->> [x] x)]
                     (-> (->> x) inc)))]
       x := '(let* [[foo bar baz ->>]
                    (:hyperfiddle.electric.impl.compiler/letfn [foo (fn* foo ([with-open] (with-open 1)))
                                                            bar (fn* bar ([x] (inc x)))
                                                            baz (fn* baz ([x] (->> x)))
                                                            ->> (fn* ->> ([x] x))])]
               (inc (->> x)))
       (has-line-meta? x) := true)

     (let [[f v :as x] (all '(set! (.-x (-> [(java.awt.Point. (-> 0 inc) 2)] first)) (-> 2 inc)))
           fnbody (-> f second second butlast)] ; to extract (fn* ([gensym] -this-> (set! .. gensym)))
       fnbody := '(set! (. (first [(new java.awt.Point (inc 0) 2)]) -x))
       v := '(inc 2)
       (has-line-meta? x) := true)

     (all '(new java.awt.Point (-> 0 inc) 1)) := '(new java.awt.Point (inc 0) 1)
     (all '(java.awt.Point. (-> 0 inc) 1)) := '(new java.awt.Point (inc 0) 1)
     (all '(new (missionary.core/seed [(-> 0 inc)]))) := '(new (missionary.core/seed [(inc 0)]))

     ;; TODO next iteration
     ;; (all '(try (-> 1 inc)
     ;;            (catch Throwable with-open (with-open 1))
     ;;            (finally (-> 0 dec))))
     ;; := '(try (inc 1)
     ;;          (catch Throwable with-open (with-open 1))
     ;;          (finally (dec 0)))

     ;; (all '(try true)) := '(try true)
     ;; ;; works outside RCF
     ;; ;; (let [with-open inc] (all '(with-open 1)))
     ;; ;; := '(with-open 1)

     ;; (all '(catch (-> 1 inc))) := '(catch (inc 1))

     (let [x (all '(loop [with-open inc, x 2] (-> x with-open)))]
       x := `(~'binding [c/rec
                         (::c/closure
                          (let* [~'with-open r/%0, ~'x r/%1]
                            (~'with-open ~'x)))]
              (new c/rec ~'inc 2))
       (has-line-meta? x) := true)

     (let [x (all '(binding [x (-> 1 inc)] (-> x inc)))]
       x := '(binding [x (inc 1)] (inc x))
       (has-line-meta? x) := true)

     (let [x (all '((-> inc) 1))]
       x := '(inc 1)
       (has-line-meta? x) := true)

     (all '()) := '()

     (all '(hyperfiddle.impl.expand-test/X.)) := '(new hyperfiddle.impl.expand-test/X)

     (c/-expand-all '(#{:ok} 1) {:js-globals {}})

     "cljs var lookup doesn't produce undeclared-ns warnings"
     (let [!warns (atom [])]
       (cljs.env/ensure
         (cljs.analyzer/with-warning-handlers [(fn [typ env extra]
                                                 (when (typ cljs.analyzer/*cljs-warnings*)
                                                   (swap! !warns conj [typ env extra])))]
           (binding [*err* *out*]
             (with-out-str (c/-expand-all '(r/reflect 1) {::c/peers {:client :cljs, :server :clj} ::c/current :client})))))
       @!warns := [])

     "expansion is peer-aware"
     (c/expand-all {::c/peers {:client :cljs, :server :clj}, ::c/current :server}
       `[(test-peer-expansion) (::c/toggle :client {} (test-peer-expansion))])
     := `[:clj (::c/toggle :client {} :cljs)]

     (c/expand-all {::c/peers {:client :cljs, :server :clj}, ::c/current :client}
       `[(test-peer-expansion) (::c/toggle :server {} (test-peer-expansion))])
     := `[:cljs (::c/toggle :server {} :clj)]

     "cljs require-macros work in clj expansion"
     (c/expand-all {::c/peers {:client :cljs, :server :clj}, ::c/current :client, :ns 'hyperfiddle.electric.impl.expand-test}
       '(hyperfiddle.electric.impl.expand-macro/twice 1))
     := '[1 1]
     (c/expand-all {::c/peers {:client :cljs, :server :clj}, ::c/current :client, :ns 'hyperfiddle.electric.impl.expand-test}
       '(mac/twice 1))
     := '[1 1]
     (c/expand-all {::c/peers {:client :cljs, :server :clj}, ::c/current :client, :ns 'hyperfiddle.electric.impl.expand-test}
       '(twice 1))
     := '[1 1]

     "require referred macros work in cljs"
     (c/expand-all {::c/peers {:client :cljs, :server :clj}, ::c/current :client, :ns 'hyperfiddle.electric.impl.expand-test}
       '(referred))
     := :referred

     "required macros work in cljs when fully qualified"
     (c/expand-all {::c/peers {:client :cljs, :server :clj}, ::c/current :client, :ns 'hyperfiddle.electric.impl.expand-test}
       '(hyperfiddle.electric.impl.expand-require-referred/referred))
     := :referred

     "required macros work in cljs when alias qualified"
     (c/expand-all {::c/peers {:client :cljs, :server :clj}, ::c/current :client, :ns 'hyperfiddle.electric.impl.expand-test}
       '(ref/referred))
     := :referred

     (println " ok")))

;; doesn't work in `tests`
#?(:clj
   (when-not (= '(let* [x 1])
               (binding [*ns* (create-ns 'hyperfiddle.electric.impl.expand-unloaded)]
                 (c/expand-all {::c/peers {:client :cljs, :server :clj}
                              ::c/current :server, ::c/me :client
                              :ns 'hyperfiddle.electric.impl.expand-unloaded}
                   '(let [x 1]))))
     (throw (ex-info "clj macroexpansion for unloaded ns fails" {}))))
