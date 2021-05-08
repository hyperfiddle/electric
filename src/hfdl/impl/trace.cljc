(ns hfdl.impl.trace
  (:require [hfdl.impl.runtime :as r]
            [hfdl.impl.util :as u]
            [hfdl.impl.switch :refer [switch]]
            [missionary.core :as m]
            [minitest :refer [tests]])
  #?(:cljs (:require-macros [minitest])))

(def events (u/monoid u/map-into [{} #{} #{}]))
(def change (partial assoc (events) 0))
(def create (partial assoc (events) 1))
(def sample (partial assoc (events) 2))

(def peer
  (let [nodes (int 0)                                       ;; {node signal}
        inputs (int 1)                                      ;; {node cb}
        create-cb (int 2)
        sample-cb (int 3)
        local u/pure
        input (fn [s] (->> s (m/observe) (m/relieve {})))
        variable (fn [c f] (switch (m/latest (partial r/bind-context c) f)))]
    (fn [resolve boot trace >read]
      (m/reactor
        (let [process (object-array 4)
              global (fn [k]
                       (if-some [[_ v] (find resolve k)]
                         (u/pure v) (u/failer (ex-info "Unable to resolve global." {:global k}))))]
          (letfn [(context [{:keys [expression statements]} n t]
                    ((aget process create-cb) (second statements))
                    (reduce graph! nil (first statements))
                    (reduce eager! nil (first statements))
                    (graph! nil expression)
                    (((aget process nodes) expression) n t))
                  (listener [slot]
                    (->> (fn [!] (aset process slot !) u/nop)
                      (m/observe)
                      (m/relieve into)))
                  (eager! [_ node]
                    (m/stream! (get (aget process nodes) node)))
                  (spawn! [node ctor arg deps]
                    (reduce graph! nil deps)
                    (let [store (aget process nodes)]
                      (->> deps
                        (map store)
                        (apply ctor arg)
                        (u/log-failure)
                        (m/signal!)
                        (assoc store node)
                        (aset process nodes))))
                  (graph! [_ node]
                    (when (nil? (get (aget process nodes) node))
                      (let [[op & args] node]
                        (case op
                          :remote (let [id (first args)]
                                    (spawn! node input
                                      (fn [!]
                                        (aset process inputs
                                          (assoc (aget process inputs)
                                            id !)) u/nop) ())
                                    ((aget process sample-cb) #{id}))
                          :local (spawn! node local (first args) ())
                          :global (spawn! node global (first args) ())
                          :constant (spawn! node u/call u/pure args)
                          :variable (spawn! node variable context args)
                          :apply (spawn! node m/latest u/call (apply cons args))))))
                  (touch! [_ n x] ((get (aget process inputs) n) x))]
            (->> (m/ap
                   (m/amb=
                     (create (m/?> (listener create-cb)))
                     (sample (m/?> (listener sample-cb)))
                     (do (r/with-ctx context (boot))
                         (let [[changed created sampled]
                               (->> >read
                                 (r/bind-context context)
                                 (m/stream!) (m/?>))]
                           (reduce graph! nil created)
                           (reduce eager! nil created)
                           (reduce graph! nil sampled)
                           (reduce-kv touch! nil changed)
                           (let [node (m/?= (m/seed sampled))]
                             (change {node (m/?> (get (aget process nodes) node))}))))))
              (m/relieve u/map-into)
              (m/stream!)
              (u/foreach trace)
              (r/bind-context context)
              (m/stream!))))))))
