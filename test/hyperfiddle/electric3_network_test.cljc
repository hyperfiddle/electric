(ns hyperfiddle.electric3-network-test
  (:require [clojure.test :as t]
            [contrib.data :refer [->box]]
            [hyperfiddle.electric-local-def3 :as l]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.incseq :as i]
            [hyperfiddle.kvs :as kvs]
            [missionary.core :as m]
            [hyperfiddle.electric.impl.runtime3 :as r]
            [hyperfiddle.electric.impl.lang3 :as lang]
            [clojure.test.check.properties :as tc-prop]
            [clojure.test.check.generators :as tc-gen]
            [clojure.test.check.clojure-test :as tct])

  #?(:cljs (:require-macros [hyperfiddle.electric3-network-test :refer [with-electric]]))
  (:import [missionary Cancelled]
           #?(:clj [clojure.lang ExceptionInfo])))

(declare tap step)
(defmacro with-electric [[tap step] opts eform & body]
  (let [ngn (gensym "engine")]
    `(let [~ngn (l/->engine ~opts), ~tap #(l/tap ~ngn %), ~step #(l/step ~ngn %)]
       (t/testing (str "[seed " (-> ~ngn l/->info :seed) "]")
         (l/spawn ~ngn (l/local-ngn ~opts ~ngn ~eform))
         ~@body
         ~(when (= :full (:debug opts)) `(prn 'cancel-engine))
         ~@(if (:skip-cancel opts)
             `[(t/is (= 1 1))]
             `[(l/cancel ~ngn)
               (t/is (= ::l/cancelled (l/step ~ngn (constantly true))))
               #_(let [[t# ret#] (try [:ok (l/step ~ngn (constantly true))]
                                    (catch ~(if (:js-globals &env) :default `Throwable) e# [:ex e#]))]
                 (t/is (= :ex t#))
                 (t/is (instance? Cancelled ret#)))]))
       #_(t/is (~'thrown? Cancelled (l/step ~ngn (constantly true)))))))

(t/deftest simple-transfer
  (with-electric [tap step] {} (tap (e/server 1))
    (step #{1})))

(t/deftest if-glitch
  (dotimes [_ 10]
    (let [!x (atom true)]
      (with-electric [tap step] {}
          (e/client
            (let [x (e/watch !x)]
              (if x
                (let [y (tap x)]
                  (e/server (identity y)))
                (tap 'not-x))))
          (step #{true})
          (swap! !x not)
          (step #{'not-x})))))

(t/deftest if-glitch-2
  (dotimes [_ 10]
    (let [!x (atom true)]
      (with-electric [tap step] {}
          (e/client
            (let [x (e/watch !x)]
              (if x
                (e/server (tap ['x x]))
                (tap 'not-x))))
          (step #{['x true]})
          (swap! !x not)
          (step #{'not-x})))))

(t/deftest there-and-back
  (with-electric [tap step] {} (tap (e/server (e/call (e/fn [] (e/client 2)))))
    (step #{2})))

(e/defn foo [])
(t/deftest binding-foo
  (with-electric [tap step] {} (tap (binding [foo 2] (e/server (e/call (e/fn [] (e/client (identity foo)))))))
    (step #{2})))

(e/defn Bar1 [])
(t/deftest binding-foo-Bar
  (with-electric [tap step] {} (tap (binding [Bar1 (e/fn [] (e/client (identity foo))), foo 2]
                                      (e/server (e/call Bar1))))
    (step #{2})))

(e/defn state [])
(t/deftest nested-efor-with-transfer
  (let [!state (atom [1])]
    (with-electric [tap step] {} (binding [state (e/watch !state)]
                                   (e/for-by identity [x (e/server state)]
                                     (e/for-by identity [y (e/server state)]
                                       (tap [x y]))))
      (step #{[1 1]})
      (reset! !state [3])
      (step #{[3 3]}))))

(t/deftest fn-destructuring
  (with-electric [tap step] {} (do (tap (e/client ((fn [{:keys [a] ::keys [b]}] [::client a b]) {:a 1 ::b 2})))
                                   (tap (e/server ((fn [{:keys [a] ::keys [b]}] [::server a b]) {:a 1 ::b 2}))))
    (step #{[::client 1 2]})
    (step #{[::server 1 2]})))

(t/deftest switch
  (let [!x (atom true)]
    (with-electric [tap step] {} (let [x (e/watch !x)] (tap (if x (e/server [:server x]) [:client x])))
      (step #{[:server true]})
      (swap! !x not)
      (step #{[:client false]}))))

(t/deftest push-on-both
  (with-electric [tap step] {} (let [foo 1] (tap foo) (tap (e/client (identity foo))))
    (step #{1})
    (step #{1})))

(t/deftest efor-with-remote-body
  (let [!offset (atom 0)]
    (with-electric [tap step] {} (e/for [j (let [o (e/watch !offset)]
                                             (e/diff-by identity
                                               (range o (+ o 2))))]
                                   (e/server (tap j)))
      (step #{0 1})
      (step #{0 1})
      (swap! !offset inc)
      (step #{2}))))

(defn mount-at [kvs k v]
  (m/observe
    (fn [!]
      (! (i/empty-diff 0))
      (kvs/insert! kvs k v)
      #(kvs/remove! kvs k))))

(t/deftest mount-point
  (with-electric [tap step] {:skip-cancel true} ; with cancellation a [] sneaks through
      (let [mp (e/mount-point)]
        (tap (e/as-vec (e/join mp)))
        (e/server
          (e/call
              (e/fn []
                (e/client
                  [(e/join (mount-at mp (e/tag) :foo))
                   (e/join (mount-at mp (e/tag) :bar))])))))
      (step #{[]})
      (step #{[:foo :bar]})))

(t/deftest branch-unmount
  (let [!x (atom true)]
    (with-electric [tap step] {} (if (e/watch !x) (e/server (tap :branch)) (tap :unmount))
      (step #{:branch})
      (swap! !x not) (step #{:unmount})
      (swap! !x not) (step #{:branch})
      (swap! !x not) (step #{:unmount}))))

(t/deftest server-client-server
  (with-electric [tap step] {} (e/server (tap (e/client (identity (e/server :foo)))))
    (step #{:foo})))

;; does not repro nondet bug found and fixed by Leo (electric3 test with clocks near the end)
(t/deftest do-not-dispose-convicted-outputs
  (dotimes [_ 10]
    (let [!x (atom 0)
          !y (atom true)
          !z (atom true)]
      (with-electric [tap step] {} (let [x (e/watch !x)]
                                     (tap (when (e/watch !y)
                                            (e/server [(identity x)
                                                       (when (e/watch !z) x)]))))
        (step #{[0 0]})
        (swap! !z not)
        (step #{[0 nil]})
        (swap! !y not)
        (step nil?)
        ))))

(t/deftest e-for-on-server-body-on-client-double-client-change
  (dotimes [_ 10]
    (let [!x (atom 0)]
      (with-electric [tap step] {} (e/server (let [x (e/watch !x)] (e/for [x x] (e/client (tap x)))))
        (step #{0})
        (swap! !x inc)
        (swap! !x inc)
        (when (= 1 (step #{1 2}))
          (swap! !x inc)
          (when (= 2 (step #{2 3}))
            (step #{3})))
        ))))
(t/deftest e-for-on-server-body-on-client
  (dotimes [_ 10]
    (let [!x (atom 1)]
      (with-electric [tap step] {:skip-cancel true}
          (e/server
            (e/for [x (e/diff-by {} (range (e/watch !x)))]
              (e/client (r/do! (tap x) (e/on-unmount #(tap [:bye x]))))))
          (step #{0})
          (swap! !x inc)
          (swap! !x inc)
          (step #{1 2})
          (step #{1 2})
          (reset! !x 0)
          (step #{[:bye 0] [:bye 1] [:bye 2]})
          (step #{[:bye 0] [:bye 1] [:bye 2]})
          (step #{[:bye 0] [:bye 1] [:bye 2]})
          (swap! !x inc)
          (step #{0})
          ))))
(t/deftest e-for-on-server-body-on-client-shrink-to-0-debug
  (dotimes [_ 10]
    (let [!x (atom 1)]
      (with-electric [tap step] {:skip-cancel true}
          (e/server
            (e/for [x (e/diff-by {} (range (e/watch !x)))]
              (e/client (r/do! (tap x) (e/on-unmount #(tap [:bye x]))))))

        (step #{0})
        (swap! !x dec)
        (step #{[:bye 0]})
        (swap! !x inc)
        (step #{0})))))

(comment
  (let [<s> (->box)]
    (dotimes [_ 100]
      (let [q (queue), ngn (l/->engine {})]
        (try (l/spawn ngn (l/local-ngn {} ngn (q (e/server (e/call (e/fn [] (e/client 2)))))))
             (l/step ngn)
             (q)
             (catch #?(:clj Throwable :cljs :default) e
               (let [[n0] (<s>), {:keys [steps seed]} (l/->info ngn)]
                 (when (or (nil? n0) (< steps n0)) (<s> [steps seed])))))))
    (<s>))
  )

;; property based codegen tests

(defn tf-server [form] `(e/server (identity ~form)))
(defn tf-client [form] `(e/client (identity ~form)))

(defn assert= [v pred-v]
  (when-not (= v pred-v) (throw (ex-info (str v " not= " pred-v) {})))
  v)

(defn tf-case-server [form]
  (let [x (gensym "x")]
    `(let [~x ~form] (case ~(tf-server x) 42 (assert= ~x 42) 43 (assert= ~x 43) #_else 0))))

(defn tf-case-client [form]
  (let [x (gensym "x")]
    `(let [~x ~form] (case ~(tf-client x) 42 (assert= ~x 42) 43 (assert= ~x 43) #_else 0))))

(defn tf-e-for [form]
  `(e/for [~'x ~form] ~'x))

(defn tf-shrink-grow-server [form]
  `(e/for [~'x (e/server (e/diff-by identity (e/as-vec ~form)))] ~'x))

(defn tf-shrink-grow-client [form]
  `(e/for [~'x (e/client (e/diff-by identity (e/as-vec ~form)))] ~'x))

(defn wrap-with-check [form x1 x2]
  `(let [~'!x (atom 42)]
     (with-electric [~'tap ~'step] {} (let [~'x (e/watch ~'!x)] (~'tap ~form))
       (~'step #{~x1})
       (swap! ~'!x inc)
       (~'step #{~x2}))))

(def gen-code-tfs
  (tc-gen/vector
    (tc-gen/elements [:server :client :server :client :server :client
                      :case-server :case-client
                      :e-for :shrink-grow-server :shrink-grow-client
                      ])
    1 5))

(defn transform-code [form transform-kw]
  (case transform-kw
    :server (tf-server form)
    :client (tf-client form)
    :case-server (tf-case-server form)
    :case-client (tf-case-client form)
    :e-for (tf-e-for form)
    :shrink-grow-server (tf-shrink-grow-server form)
    :shrink-grow-client (tf-shrink-grow-client form)
    ))

(def prop-program-42
  (tc-prop/for-all [tfs gen-code-tfs]
    (let [code (wrap-with-check (reduce transform-code 'x tfs) 42 43)]
      (try (eval code)
           (catch ExceptionInfo e (throw (ex-info "check failed" {:code (with-out-str (lang/pprint-source code))} e))))
      true)))

;; reproduces the conditional glitch
;; (tct/defspec program-42-spec 20 prop-program-42)

(def prop-program-42-42
  (tc-prop/for-all [tfs1 gen-code-tfs, tfs2 gen-code-tfs]
    (let [code (wrap-with-check `[~(reduce transform-code 'x tfs1)
                                  ~(reduce transform-code 'x tfs2)]
                 [42 42] [43 43])]
      (try (eval code)
           (catch ExceptionInfo e (throw (ex-info "check failed" {:code (with-out-str (lang/pprint-source code))} e))))
      true)))

;; reproduces d-glitch
;; (tct/defspec program-42-42-spec 20 prop-program-42-42)
