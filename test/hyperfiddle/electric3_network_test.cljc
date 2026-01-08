(ns hyperfiddle.electric3-network-test
  (:require [clojure.test :as t]
            [contrib.data :refer [->box]]
            [contrib.debug :as dbg]
            [contrib.walk :as walk]
            [contrib.triple-store :as ts]
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


;; sophisticated codegen:
;; - can mutate any part of the source code. E.g. a `case` has many points (test value, each branch) to mutate
;; how:
;; - given an input set of atoms (e.g. x=42, y=0)
;; - and their next state (e.g. x=10, y=1)
;; - start with some static start value, e.g. [x y]
;; - hold code in a triple store - allows arbitrary point mutations
;; - create 2 triple store readers
;;   1. generates the electric source code (s-exp)
;;   2. calculates value at point given input set (interpreter)
;; - the interpreter allows knowing what is the value at T0 and T1 so we can generate smart code
;; - e.g. if value at point is [42 0] then [10 1] we know what `case` is interesting to generate
;; generating/shrinking:
;; - start with 1 mutation, keep adding one by one up to a limit (e.g. 10 or 20)
;; - run each source with a number of network topologies (e.g. 10 or 100)
;; - on hitting a failure case try removing each mutation one by one and repeat with a number of network topologies
;; - loop over the removals until hitting a fixpoint
;; benefits:
;; - highly randomized source code = larger coverage
;; - custom generator plugs in more easily into the network testing framework
;; - custom generator allows custom (perfect) shrinking

(defn ->->id [] (let [!i (long-array [-1])] (fn [] (aset !i 0 (unchecked-inc (aget !i 0))))))

(defn add-singular-vector [{{:keys [->id inputs]} :o, :as ts} parent-id]
  (let [vector-id (->id)]
    (-> ts
      (ts/add {:db/id vector-id, :type :vector, :parent parent-id})
      (ts/add {:db/id (->id), :parent vector-id, :type :value, :value (rand-nth inputs)}))))

(defn initialize-source-ts [inputs]
  (let [->id (->->id)]
    (-> (ts/->ts {:->id ->id, :inputs inputs})
      (add-singular-vector -1))))

(e/defn Server [x] (e/server (identity x)))
(e/defn Client [x] (e/client (identity x)))

(def source-mutations
  [
   (fn m!server [{{:keys [->id]} :o, :as ts}]
     (let [server-id (->id)
           id-to-mutate (loop []
                          (let [id (rand-nth (vec (ts/key ts :type)))]
                            (if (= :server (ts/? ts id :type))
                              (recur)
                              id)))
           parent (ts/? ts id-to-mutate :parent)]
       (-> ts
         (ts/add {:db/id server-id, :parent parent, :type :server})
         (ts/asc id-to-mutate :parent server-id))))
   (fn m!add-value [{{:keys [->id inputs]} :o, :as ts}]
     (let [vector-id (rand-nth (vec (ts/find ts :type :vector)))]
       (ts/add ts {:db/id (->id), :parent vector-id, :type :value, :value (rand-nth inputs)})
       ))
   (fn m!case [{{:keys [->id]} :o, :as ts}]
     (let [case-id (->id)
           id-to-mutate (rand-nth (vec (ts/key ts :type)))
           parent (ts/? ts id-to-mutate :parent)]
       (-> ts
         (ts/add {:db/id case-id, :parent parent, :type :case})
         (ts/asc id-to-mutate :parent case-id)
         (add-singular-vector case-id)
         (add-singular-vector case-id))))
   ])

(defn mutate-source [ts]
  ((rand-nth source-mutations) ts))

(defn add-input-values [{{:keys [inputs]} :o, :as ts}]
  (update ts :o assoc
    :t0 (zipmap inputs (map #(keyword (str % 0)) inputs))
    :t1 (zipmap inputs (map #(keyword (str % 1)) inputs))))

(defn get-root-node-id [ts] (ts/find1 ts :parent -1))

(defn compute-symbolic-return-value
  ([ts time] (compute-symbolic-return-value ts time (get-root-node-id ts)))
  ([ts time node-id]
   ((fn rec [node-id]
      (let [node (ts/->node ts node-id)]
        (case (:type node)
          (:server) (rec (ts/find1 ts :parent node-id))
          (:vector) (mapv rec (ts/find ts :parent node-id))
          (:case) (let [test&branches (sort (ts/find ts :parent node-id))]
                    (rec (nth test&branches (inc time))))
          (:null) nil
          (:value) (:value node))))
    node-id)))

(defn get-mapping-in-time [ts time]
  (-> ts :o (get (case time 0 :t0 1 :t1))))

(defn compute-return-value
  ([ts time] (compute-return-value ts time (get-root-node-id ts)))
  ([ts time node-id]
   (let [sym-ret (compute-symbolic-return-value ts time node-id)
         mapping (get-mapping-in-time ts time)]
     (walk/postwalk #(cond-> % (symbol? %) (mapping %)) sym-ret))))

(defn generate-source
  ([ts] (generate-source ts #{} (get-root-node-id ts)))
  ([ts times node-id]
   (let [node (ts/->node ts node-id)]
     (case (:type node)
       (:server) `(~'Server ~(generate-source ts times (ts/find1 ts :parent node-id)))
       (:vector) `[~@(mapv #(generate-source ts times %) (sort (ts/find ts :parent node-id)))]
       (:case) (let [[test branch0 branch1] (sort (ts/find ts :parent node-id))]
                 `(~'case ~(generate-source ts times test)
                   ~(compute-return-value ts 0 test) ~(generate-source ts (conj times 0) branch0)
                   ~(compute-return-value ts 1 test) ~(generate-source ts (conj times 1) branch1)
                   :unreachable))
       (:null) nil
       (:value) (case (count times)
                  0 (:value node)
                  1 (list 'assert= (:value node) (get (get-mapping-in-time ts (first times)) (:value node)))
                  2 :unreachable)))))

(defn generate-test [ts]
  `(~'let [~'!inputs (~'atom ~(list 'quote (-> ts :o :t0))) #_ ~@(mapcat (fn [[s v]] `[~(banged s) (~'atom ~v)]) (-> ts :o :t0))]
    (~'with-electric [~'tap ~'step] {}
     (~'let [~'inputs (~'e/watch ~'!inputs)
             ~@(mapcat (fn [s] `[~s (~'get ~'inputs ~(list 'quote s)) #_(~'e/watch ~(banged s))]) (-> ts :o :inputs))]
      (~'tap ~(generate-source ts)))
     (~'step #{~(compute-return-value ts 0)})
     ~(list 'reset! '!inputs (list 'quote (-> ts :o :t1)))
     (~'step #{~(compute-return-value ts 1)}))))

(defn run-test [ts topo-test-count]
  (let [code (generate-test ts)]
    ;; (lang/pprint-source code)
    (try (dotimes [_ topo-test-count] (eval code)) [:ok]
         (catch #?(:clj Throwable :cljs :default) e
           ;; (lang/pprint-source code)
           [:ex e]))))

(defn reparent [ts parent-id node-id]
  (ts/asc ts node-id :parent parent-id))

(defn reparent* [ts parent-id node-ids]
  (reduce (fn [ts node-id] (reparent ts parent-id node-id)) ts node-ids))

(defn delete-point [ts node-id]
  (reduce delete-point (ts/del ts node-id) (ts/find ts :parent node-id)))

(defn delete-point* [ts node-ids]
  (reduce delete-point ts node-ids))

(defn raise-point [ts point-to-raise-id new-position-id]
  (let [point-to-raise (ts/->node ts point-to-raise-id)
        new-position (ts/->node ts new-position-id)
        delete-ids (disj (ts/find ts :parent new-position-id) point-to-raise-id)]
    (-> ts
      (ts/add (merge (select-keys new-position [:db/id :parent]) (dissoc point-to-raise :db/id :parent)))
      (reparent* new-position-id (ts/find ts :parent test))
      (delete-point* delete-ids))))

(defn shrink-test [ts topo-test-count ex]
  (let [run (fn [ts] (run-test ts topo-test-count))]
    ((fn rec [ts ex node-id]
       (let [node (ts/->node ts node-id)]
         (case (:type node)
           (:server) (let [child-id (ts/find1 ts :parent node-id)
                           child-node (ts/->node ts child-id)]
                       (if (= :server (:type child-node))
                         (recur (-> ts (reparent node-id (ts/find1 ts :parent child-id)) (delete-point child-id))
                           ex node-id)
                         (recur ts ex child-id)))
           (:vector) (let [[ts ex] (reduce (fn [[ts ex] node-id]
                                             (let [next-ts (delete-point ts node-id)
                                                   [t next-ex] (run next-ts)]
                                               (case t
                                                 :ex [next-ts next-ex]
                                                 :ok [ts ex])))
                                     [ts ex] (next (ts/find ts :parent node-id)))]
                       (reduce (fn [[ts ex] node-id] (rec ts ex node-id)) [ts ex] (ts/find ts :parent node-id)))
           (:case) (let [[test branch0 branch1] (sort (ts/find ts :parent node-id))
                         next-ts (raise-point ts test node-id)
                         [t next-ex] (run next-ts)]
                     (case t
                       :ex (recur next-ts next-ex node-id)
                       :ok (let [next-ts (raise-point ts branch0 node-id)
                                 [t next-ex] (run next-ts)]
                             (case t
                               :ex (recur next-ts next-ex node-id)
                               :ok (let [next-ts (raise-point ts branch1 node-id)
                                         [t next-ex] (run next-ts)]
                                     (case t
                                       :ex (recur next-ts next-ex node-id)
                                       :ok (reduce (fn [[ts ex] node-id] (rec ts ex node-id)) [ts ex] (ts/find ts :parent node-id))))))))
           #_else [ts ex])))
     ts ex (get-root-node-id ts))))

#?(:clj
   (defn run-generated-test []
     (let [ts (-> (initialize-source-ts '[a b c d])
                mutate-source mutate-source mutate-source mutate-source mutate-source
                add-input-values)
           topo-test-count 16
           [t ex] (run-test ts topo-test-count)]
       (case t
         :ok :ok
         :ex (let [[ts ex] (shrink-test ts 32 ex)]
               (lang/pprint-source (generate-test ts))
               (throw ex))))))

;; copied and further simplified from a `run-generated-test` run (d-glitch)
(defn broken []
  (let [!inputs (atom '{a :a0, b :b0})]
    (with-electric [tap step] {}
        (let [inputs (e/watch !inputs) a (get inputs 'a) b (get inputs 'b)]
          (tap [a (Server b)]))
      (step #{[:a0 :b0]})
      (reset! !inputs '{a :a1, b :b1})
      (step #{[:a1 :b1]}))))

;; copied and further simplified from a `run-generated-test` run (conditional glitch)
(defn broken-2 []
  (let [!inputs (atom '{a :a0, b :b0})]
    (with-electric [tap step] {}
        (let [inputs (e/watch !inputs) a (get inputs 'a) b (get inputs 'b)]
          (tap (case (Server b) :b0 (assert= a :a0) :b1 (assert= a :a1) :unreachable)))
      (step #{:a0})
      (reset! !inputs '{a :a1, b :b1})
      (step #{:a1}))))

(comment
  (broken)
  (broken-2)
  (run-generated-test)
  (-> (initialize-source-ts '[a b c d])
    mutate-source
    mutate-source
    ;; mutate-source
    ;; mutate-source
    ;; mutate-source
    ;; (#(do (run! (comp prn second) (:eav %)) %))
    add-input-values
    #_(#((juxt identity (fn [_] (-> % :o :t1)) (fn [v] (replace (-> % :o :t1) v))) (compute-symbolic-return-value % 1)))
    generate-test
    ;;eval
    )
  )
