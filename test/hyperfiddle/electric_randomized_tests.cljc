(ns hyperfiddle.electric-randomized-tests
  (:require [dustingetz.triple-store :as ts]
            [dustingetz.walk :as walk]
            [clojure.string :as str]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric.impl.lang3 :as lang]
            [hyperfiddle.electric3-network-test :as net-test])) ; used in codegen
(defn find-by [pred x*] (reduce (fn [_ x] (when (pred x) (reduced x))) nil x*))
;; TRIPLE STORE HELPERS
(defn ->->id [] (let [!i (long-array [-1])] (fn [] (aset !i 0 (unchecked-inc (aget !i 0))))))
(defn active-parent ([ts] (-> ts :o :p)) ([ts p] (update ts :o assoc :p p)))
(defn with-parent [ts p f] (let [prevp (active-parent ts)] (-> ts (active-parent p) f (active-parent prevp))))
(defn next-id [ts] ((-> ts :o :->id)))
(defn next-uid [ts] ((-> ts :o :->uid)))
(defn ensure-uid [ts node] (if (:uid node) node (assoc node :uid (next-uid ts))))
(defn id->uid [ts id] (ts/? ts id :uid))
(defn uid->id [ts uid] (ts/find1 ts :uid uid))
(defn uid->node [ts uid] (ts/->node ts (ts/find1 ts :uid uid)))
(defn id->p [ts id] (ts/? ts id :p))
(defn add-node [ts base]
  (ts/add ts (cond-> base
               (not (contains? base :p))     (assoc :p (active-parent ts))
               (not (contains? base :db/id)) (assoc :db/id (next-id ts))
               (not (contains? base :uid))   (assoc :uid (next-uid ts)))))
(defn under [ts base f]
  (let [base (ensure-uid ts base)]
    (-> ts (add-node base) (with-parent (:uid base) f))))
(defn input-syms [ts] (mapv :name$ (-> ts :o :inputs)))
(defn random-singular-vector-node [ts] {:t :v, :v (rand-nth (input-syms ts))})
(defn add-random-singular-vector [ts] (under ts {:t :vector} (fn [ts] (add-node ts (random-singular-vector-node ts)))))
(defn random-node-id ([ts] (rand-nth (vec (ts/key ts :t)))) ([ts t] (rand-nth (vec (ts/find ts :t t)))))
(defn children [ts uid] (sort (ts/find ts :p uid)))
(defn child [ts uid] (ts/find1 ts :p uid))
(defn parent [ts uid] (ts/find1 ts :uid (:p (uid->node ts uid))))
(defn wrap-node [ts id-to-wrap wrapper]
  (let [node-to-wrap (ts/->node ts id-to-wrap)
        wrapper (assoc (ensure-uid ts wrapper) :db/id id-to-wrap)]
    (with-parent (ts/del ts id-to-wrap) (:p node-to-wrap)
      (fn [ts] (under ts wrapper (fn [ts] (add-node ts (dissoc node-to-wrap :db/id :p))))))))
(defn delete-recursively [ts node-id] (reduce delete-recursively (ts/del ts node-id) (children ts (id->uid ts node-id))))
(defn raise [ts node-id]
  (let [node (ts/->node ts node-id)
        parent-node (ts/->node ts (parent ts (:uid node)))
        parent-children (children ts (:uid parent-node))
        parent-children-to-delete (disj (set parent-children) node-id)]
    (-> (reduce delete-recursively ts parent-children-to-delete)
      (ts/del (:db/id parent-node))
      (ts/del node-id)
      (ts/add (merge node (select-keys parent-node [:db/id :p]))))))
;; CODEGEN
(e/defn Server [x] (e/server (identity x)))
(e/defn Client [x] (e/client (identity x)))
(defn assert= [v pred-v] (when-not (= v pred-v) (throw (ex-info (str v " not= " pred-v) {}))) v)
(defn m!server [ts]
  (let [id-to-wrap (->> (repeatedly #(random-node-id ts)) (find-by #(not= :server (ts/? ts % :t))))]
    (wrap-node ts id-to-wrap {:t :server})))
(defn m!add-vec-value [ts]
  (with-parent ts (->> (random-node-id ts :vector) (id->uid ts))
    (fn [ts] (add-node ts {:t :v, :v (rand-nth (input-syms ts))}))))
(defn m!case [ts]
  (let [id-to-wrap (random-node-id ts), node-to-wrap (ts/->node ts id-to-wrap)]
    (with-parent (ts/del ts id-to-wrap) (:p node-to-wrap)
      (fn [ts]
        (under ts {:t :case, :db/id id-to-wrap}
          (fn [ts]
            (-> ts
              (add-node (dissoc node-to-wrap :db/id :p))
              (add-random-singular-vector)
              (add-random-singular-vector))))))))
(def source-mutations [m!server m!add-vec-value m!case])
(defn mutate-source [ts] ((rand-nth source-mutations) ts))
(def root-uid -1)
(defn get-root-node-id [ts] (ts/find1 ts :p root-uid))
(defn init [{:keys [inputs] :as opts}]
  (let [ts (ts/->ts {:->id (->->id), :->uid (->->id), :p root-uid,
                     :inputs inputs, :run-opts (dissoc opts :inputs)})]
    (add-random-singular-vector ts)))
(defn value-at-time [_ts name$ t] (keyword (str name$ t)))
(defn compute-symbolic-return-value [ts node-id t]
  ((fn rec [node-id]
     (let [node (ts/->node ts node-id)]
       (case (:t node)
         (:server) (rec (child ts (:uid node)))
         (:vector) (mapv rec (children ts (:uid node)))
         (:case) (rec (nth (children ts (:uid node)) (inc t)))
         (:v) (:v node))))
   node-id))
(defn compute-return-value
  ([ts t] (compute-return-value ts (get-root-node-id ts) t))
  ([ts node-id t] (walk/postwalk #(if (symbol? %) (value-at-time ts % t) %) (compute-symbolic-return-value ts node-id t))))
(defn generate-source
  ([ts] (generate-source ts #{} (get-root-node-id ts)))
  ([ts times node-id]
   (let [node (ts/->node ts node-id)]
     (case (:t node)
       (:server) (list 'Server (generate-source ts times (child ts (:uid node))))
       (:vector) (mapv #(generate-source ts times %) (children ts (:uid node)))
       (:case) (let [[test branch0 branch1] (children ts (:uid node))]
                 `(~'case ~(generate-source ts times test)
                   ~(compute-return-value ts test 0) ~(generate-source ts (conj times 0) branch0)
                   ~(compute-return-value ts test 1) ~(generate-source ts (conj times 1) branch1)
                   :unreachable))
       (:v) (case (count times)
              0 (:v node)
              1 (list 'assert= (:v node) (value-at-time ts (:v node) (first times)))
              #_else :unreachable)
       #_else (throw (ex-info (str "don't know how to generate " node) node))))))
(defn mksym [& args] (symbol (apply str (mapv #(if (keyword? %) (name %) (str %)) args))))
;; suffixes  %=keyword $=symbol
(defn gen-let-input [ts {:keys [name$]}] [(mksym '! name$) (list 'atom (value-at-time ts name$ 0))])
(defn gen-let-input* [ts] (mapcat (partial gen-let-input ts) (-> ts :o :inputs)))
(defn gen-let-input-watch [{:keys [name$ site%]}] [name$ `(~(mksym "e/" site%) (~'e/watch ~(mksym '! name$)))])
(defn gen-let-input-watch* [ts] (mapcat gen-let-input-watch (-> ts :o :inputs)))
(defn gen-input-reset [ts t {:keys [name$]}] (list 'reset! (mksym '! name$) (value-at-time ts name$ t)))
(defn gen-input-reset* [ts t] (mapv (partial gen-input-reset ts t) (-> ts :o :inputs)))
(defn generate-test [ts]
  `(let [~@(gen-let-input* ts)]
     (~'net-test/with-electric [~'tap ~'step] {}
      (~'let [~@(gen-let-input-watch* ts)]
       (~'tap ~(generate-source ts)))
      (~'step #{~(compute-return-value ts 0)})
      ~@(gen-input-reset* ts 1)
      (~'step #{~(compute-return-value ts 1)}))))
(defn run1 [ts]
  (let [code (generate-test ts)
        code-fn (eval `(fn [] ~code))]
    (try (dotimes [_ (:topo-test-count (-> ts :o :run-opts) 16)] (code-fn)) nil
         (catch #?(:clj Throwable :cljs :default) ex ex))))
;; SHRINKING
(defn case-test-id [ts case-node-uid] (-> (children ts case-node-uid) first))
(defn case-branch0-id [ts case-node-uid] (-> (children ts case-node-uid) second))
(defn case-branch1-id [ts case-node-uid] (-> (children ts case-node-uid) last))
(def invariants [{:name :case-test-distinct
                  :pred (fn i!case-test-distinct [ts]
                          (every? (fn [case-id]
                                    (let [test-id (case-test-id ts (id->uid ts case-id))]
                                      (not= (compute-return-value ts test-id 0) (compute-return-value ts test-id 1))))
                            (ts/find ts :t :case)))}])
(defn find-violation [ts] (some (fn [{:keys [name pred]}] (when-not (pred ts) name)) invariants))
(defn try-shrink [ts f]
  (let [shrunk-ts (f ts)]
    (when-not (find-violation shrunk-ts)
      (when-some [ex (run1 shrunk-ts)]
        [shrunk-ts ex]))))
(defn partition-into [v n]
  (let [size (count v)]
    (mapv (fn [i] (subvec v (quot (* i size) n) (quot (* (inc i) size) n))) (range n))))
(defn complement-at [chunks i] (into [] cat (concat (take i chunks) (drop (inc i) chunks))))
(defn ddmin [c0 p min-size]
  (loop [c c0, ex nil, n 2]
    (let [size (count c)]
      (if (<= size min-size)
        (when ex [c ex])
        (let [n (min n size), chunks (partition-into c n)
              probe (fn [subset]
                      (when (>= (count subset) min-size)
                        (when-some [ex2 (p subset)] [subset ex2])))]
          (if-some [[c2 ex2] (some probe chunks)]
            (recur c2 ex2 2)
            (if-some [[c2 ex2] (when (> n 2) (some #(probe (complement-at chunks %)) (range n)))]
              (recur c2 ex2 (max (dec n) 2))
              (if (< n size) (recur c ex (min (* 2 n) size)) (when ex [c ex])))))))))
(defn vector-keep [ts vec-uid keep-cids]
  (let [ks (set keep-cids)]
    (reduce (fn [ts cid] (if (ks cid) ts (delete-recursively ts cid))) ts (children ts vec-uid))))
(defn raise-vector-child [ts vec-uid]
  (some (fn [cid] (try-shrink ts (fn [ts] (raise ts cid)))) (children ts vec-uid)))
(defn shrink-vector [ts vec-uid]
  (let [cs (vec (children ts vec-uid))]
    (when (> (count cs) 1)
      (when-some [[keep ex] (ddmin cs
                              (fn [keep-cids]
                                (let [ts2 (vector-keep ts vec-uid keep-cids)]
                                  (when-not (find-violation ts2) (run1 ts2))))
                              1)]
        [(vector-keep ts vec-uid keep) ex]))))
(defn shrink-node [ts ex node-id]
  (or (when-some [node (ts/->node ts node-id)]
        (case (:t node)
          :server (try-shrink ts (fn [ts] (raise ts (child ts (:uid node)))))
          :vector (let [vec-uid (:uid node)]
                    (or (raise-vector-child ts vec-uid) (shrink-vector ts vec-uid)))
          :case (let [case-uid (:uid node)]
                  (or (try-shrink ts (fn [ts] (raise ts (case-test-id ts case-uid))))
                      (try-shrink ts (fn [ts] (raise ts (case-branch0-id ts case-uid))))
                      (try-shrink ts (fn [ts] (raise ts (case-branch1-id ts case-uid))))))
          :v (let [first-sym (first (input-syms ts))]
               (when-not (= first-sym (:v node))
                 (try-shrink ts (fn [ts] (ts/asc ts node-id :v first-sym)))))))
      [ts ex]))
(defn shrink-dfs [ts ex node-id]
  (if (ts/->node ts node-id)
    (let [child-ids (children ts (id->uid ts node-id))
          [ts1 ex1] (reduce (fn [[ts ex] cid] (shrink-dfs ts ex cid))
                      [ts ex] child-ids)
          [ts2 ex2] (shrink-node ts1 ex1 node-id)]
      (if (not= (:eav ts1) (:eav ts2))
        (shrink-dfs ts2 ex2 node-id)
        [ts2 ex2]))
    [ts ex]))
(defn enter-shrink-mode [ts] (update-in ts [:o :run-opts] (fn [o] (assoc o :topo-test-count (:shrink-topo-test-count o 64)))))
(defn shrink [ts ex]
  (let [ts (enter-shrink-mode ts)]
    (loop [ts ts, ex ex, n (:max-iterations (-> ts :o :run-opts) 3)]
      (let [[ts2 ex2] (shrink-dfs ts ex (get-root-node-id ts))]
        (if (or (zero? n) (= (:eav ts) (:eav ts2)))
          [ts ex]
          (recur ts2 ex2 (dec n)))))))
;; SAMPLE RUNNER
(defn ex-message-stack [ex] (into [] (comp (take-while some?) (map ex-message)) (iterate ex-cause ex)))
(defn ->fail-exception [ts ex]
  (let [code (generate-source ts)]
    (ex-info (str "test failed: " (str/join ": " (ex-message-stack ex)) "\n\n  " (with-out-str (lang/pprint-code code)))
      {:code code} ex)))
(defn run-test [opts]
  (let [ts (-> (init (assoc opts :inputs '[{:site% :client, :name$ c}]))
             mutate-source mutate-source mutate-source mutate-source mutate-source)]
    (if-some [ex (run1 ts)]
      (do #_(lang/pprint-code (generate-source ts))
          (let [[ts ex] (shrink ts ex)]
            {:t :ex, :ex (->fail-exception ts ex)}))
      {:t :ok})))
(comment
  (some-> (run-test {}) :ex ex-message println)
  )
