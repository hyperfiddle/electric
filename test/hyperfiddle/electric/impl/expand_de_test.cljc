(ns hyperfiddle.electric.impl.expand-de-test
  (:require #?(:clj [cljs.env])
            #?(:clj [cljs.analyzer])
            #?(:clj [hyperfiddle.electric.impl.lang-de2 :as l])
            #?(:clj [hyperfiddle.electric.impl.runtime-de :as r])
            #?(:clj [hyperfiddle.electric :as-alias e])
            [hyperfiddle.electric.impl.expand-require-referred :as ref :refer [referred]]
            #?(:clj [hyperfiddle.rcf :as rcf :refer [tests]]))
  #?(:cljs (:require-macros [hyperfiddle.electric.impl.expand-macro :as mac :refer [twice]])))

#?(:clj
   (defmacro all [o] `(l/expand-all ~(if (:js-globals &env)
                                       (assoc &env ::l/peers {:client :cljs, :server :cljs}, ::l/current :client)
                                       {:locals &env, ::l/peers {:client :clj, :server :clj}, ::l/current :client})
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

     "implicit `do`s expand. Electric is pure"
     (all '(let [] 1 2)) := (all '(let [] (do 1 2)))
     (all '(loop [] 1 2)) := (all '(loop [] (do 1 2)))
     (all '(fn [] 1 2)) := (all '(fn [] (do 1 2)))
     (all '(letfn [] 1 2)) := (all '(letfn [] (do 1 2)))
     (all '(binding [] 1 2)) := (all '(binding [] (do 1 2)))


     (all '(inc (let [x 1] x))) := '(inc (let* [x 1] x))

     (all '(let [with-open inc] (with-open 1))) := '(let* [with-open inc] (with-open 1))
     (all '(let [with-open inc, x (with-open inc)] x)) := '(let* [with-open inc, x (with-open inc)] x)

     (all '(case (-> 1 inc) (2) (-> 2 inc) (with-open) 3 4)) := '(case (inc 1) (2) (inc 2) (with-open) 3 4)
     (has-line-meta? (all '(case (-> 1 inc) (2) (-> 2 inc) (with-open) 3 4))) := true

     (all '(if 1 2 3)) := '(case 1 (nil false) 3 2)
     (has-line-meta? (all '(if 1 2 3))) := true

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
                    (::l/letfn [foo (fn* foo ([with-open] (with-open 1)))
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
       x := `(~'binding [r/rec
                         (::l/closure
                          (let* [~'with-open r/%0, ~'x r/%1]
                            (~'with-open ~'x)))]
              (new r/rec ~'inc 2))
       (has-line-meta? x) := true)

     (let [x (all '(binding [x (-> 1 inc)] (-> x inc)))]
       x := '(binding [x (inc 1)] (inc x))
       (has-line-meta? x) := true)

     (let [x (all '((-> inc) 1))]
       x := '(inc 1)
       (has-line-meta? x) := true)

     (all '()) := '()

     (all '(hyperfiddle.impl.expand-test/X.)) := '(new hyperfiddle.impl.expand-test/X)

     (l/-expand-all '(#{:ok} 1) {:js-globals {}})

     "cljs var lookup doesn't produce undeclared-ns warnings"
     (let [!warns (atom [])]
       (cljs.env/ensure
         (cljs.analyzer/with-warning-handlers [(fn [typ env extra]
                                                 (when (typ cljs.analyzer/*cljs-warnings*)
                                                   (swap! !warns conj [typ env extra])))]
           (binding [*err* *out*]
             (with-out-str (l/-expand-all '(r/reflect 1) {::l/peers {:client :cljs, :server :clj} ::l/current :client})))))
       @!warns := [])

     "expansion is peer-aware"
     (l/expand-all {::l/peers {:client :cljs, :server :clj}, ::l/current :server}
       `[(test-peer-expansion) (::l/site :client (test-peer-expansion))])
     := `[:clj (::l/site :client :cljs)]

     (l/expand-all {::l/peers {:client :cljs, :server :clj}, ::l/current :client}
       `[(test-peer-expansion) (::l/site :server (test-peer-expansion))])
     := `[:cljs (::l/site :server :clj)]

     "cljs require-macros work in clj expansion"
     (l/expand-all {::l/peers {:client :cljs, :server :clj}, ::l/current :client, :ns {:name 'hyperfiddle.electric.impl.expand-de-test}}
       '(hyperfiddle.electric.impl.expand-macro/twice 1))
     := '[1 1]
     (l/expand-all {::l/peers {:client :cljs, :server :clj}, ::l/current :client, :ns {:name 'hyperfiddle.electric.impl.expand-de-test}}
       '(mac/twice 1))
     := '[1 1]
     (l/expand-all {::l/peers {:client :cljs, :server :clj}, ::l/current :client, :ns {:name 'hyperfiddle.electric.impl.expand-de-test}}
       '(twice 1))
     := '[1 1]

     "require referred macros work in cljs"
     (l/expand-all {::l/peers {:client :cljs, :server :clj}, ::l/current :client, :ns {:name 'hyperfiddle.electric.impl.expand-de-test}}
       '(referred))
     := :referred

     "required macros work in cljs when fully qualified"
     (l/expand-all {::l/peers {:client :cljs, :server :clj}, ::l/current :client, :ns {:name 'hyperfiddle.electric.impl.expand-de-test}}
       '(hyperfiddle.electric.impl.expand-require-referred/referred))
     := :referred

     "required macros work in cljs when alias qualified"
     (l/expand-all {::l/peers {:client :cljs, :server :clj}, ::l/current :client, :ns {:name 'hyperfiddle.electric.impl.expand-de-test}}
       '(ref/referred))
     := :referred

     (println " ok")))

;; doesn't work in `tests`
#?(:clj
   (when-not (= 'let* (first
                        (binding [*ns* (create-ns 'hyperfiddle.electric.impl.expand-unloaded)]
                          (l/expand-all {::l/peers {:client :cljs, :server :clj}
                                         ::l/current :server, ::l/me :client
                                         :ns 'hyperfiddle.electric.impl.expand-unloaded}
                            '(let [x 1])))))
     (throw (ex-info "clj macroexpansion for unloaded ns fails" {}))))
