(ns dustin.compiler2
  (:require [minitest :refer [tests]]
            [missionary.core :as m]
            [dustin.trace25 :refer [from-trace!]]))

(def conjv (fnil conj []))
(def conjs (fnil conj #{}))

(defn parenting [acc parent child]
  (-> acc
    (assoc-in [child :parent] parent)
    (update-in [parent :children] conjv child)))

(defn analyze-form [nodes form]
  (let [idx (count nodes)]
    (if (coll? form)
      (let [[f & args] form]
        (case f
          fmap (reduce (fn [nodes form]
                         (let [child-index (count nodes)
                               nodes       (analyze-form nodes form)]
                           (parenting nodes idx child-index)))
                       (conj nodes
                             {:type 'fmap
                              :form form
                              :f    (first args)})
                       (next args))

          bind (let [nodes       (conj nodes
                                       {:type 'bind
                                        :form form
                                        :f    (second args)})
                     child-index (count nodes)
                     nodes       (analyze-form nodes (first args))]
                 (parenting nodes idx child-index))))
      (if (symbol? form)
        (conj nodes {:type 'user
                     :form form})
        (throw (ex-info "Unknown form." {:form form}))))))

(defn analyze [form]
  (analyze-form [] form))

(defn source-map [form]
  (reduce-kv (fn [r i x] (update r (:form x) conjs i)) {} (analyze form)))

(tests
  (analyze '(fmap + >a >b))
  :=
  '[{:type fmap, :form (fmap + >a >b), :f +, :children [1 2]}
    {:type user, :form >a, :parent 0}
    {:type user, :form >b, :parent 0}]

  (source-map '(fmap + >a >b)) :=
  '{(fmap + >a >b) #{0},
    >a             #{1},
    >b             #{2}}

  )

;;;;;;;;;;;;;
;; RUNTIME ;;
;;;;;;;;;;;;;

(defmacro amb= [& forms]
  `(case (m/?= (m/enumerate (range ~(count forms))))
     ~@(interleave (range) forms)))

(defn bind [m f]
  (m/relieve {} (m/ap (m/?! (f (m/?! m))))))

(defn trace! [tracef >effects]
  (m/stream! (m/ap (tracef (m/?? >effects)))))



;;;;;;;;;;;;;
;; EMITTER ;;
;;;;;;;;;;;;;

(defn prefixer [prefix index]
  (symbol (str prefix "_" index)))

(defn gen-trace-pairs [prefixf analyzed-ast]
  (map-indexed (fn [idx _] `{[~idx] (m/?? ~(prefixf idx))}) analyzed-ast))

(tests
  (gen-trace-pairs (partial prefixer '>node)
    '[[0 _]
      [1 _]])
  := [{[0] `(m/?? ~'>node_0)}
      {[1] `(m/?? ~'>node_1)}])

(defn gen-trace [prefixf analyzed-ast]
  `(trace! ~(prefixf 'tracef)
     (m/stream! (m/relieve merge (m/ap (amb= ~@(gen-trace-pairs prefixf analyzed-ast)))))))

(defn emit-bindings [prefixf analyzed-ast passives]
  (reverse
    (map-indexed
     (fn [idx {:keys [type form f children]}]
        (if (contains? passives idx)
          `[~(prefixf idx) (from-trace! [~idx] ~(prefixf 'replayer))]
          (case type
            bind `[~(prefixf idx) (m/signal! (bind ~(prefixf (first children)) ~f))]
            fmap `[~(prefixf idx) (m/signal! (m/latest ~f ~@(map prefixf children)))]
            user `[~(prefixf idx) (m/signal! ~form)])))
      analyzed-ast)))

(defn emit [{:keys [analyzed-ast prefix passives]
             :or   {prefix (gensym)}}]
  (let [prefixf  (partial prefixer prefix)
        bindings (mapcat identity (emit-bindings prefixf analyzed-ast passives))]
    `(fn ~(mapv prefixf ['replayer 'tracef])
       (let [~@bindings]
         ~(gen-trace prefixf analyzed-ast)))))

(tests
 (emit {:analyzed-ast (analyze '(fmap clojure.core/+ >a >b))
        :prefix       '>node
        :passives     #{1 2}})
  :=
  `(fn [~'>node_replayer ~'>node_tracef]
     (let [~'>node_2 (from-trace! [2] ~'>node_replayer)
           ~'>node_1 (from-trace! [1] ~'>node_replayer)
           ~'>node_0 (m/signal! (m/latest + ~'>node_1 ~'>node_2))]
       (trace! ~'>node_tracef (m/stream! (m/relieve merge (m/ap (amb= {[0] (m/?? ~'>node_0)}
                                                                      {[1] (m/?? ~'>node_1)}
                                                                      {[2] (m/?? ~'>node_2)}))))))))

(tests
 (emit {:analyzed-ast (analyze `(~'fmap + >a >b))
        :prefix       '>node})
  :=
  `(fn [~'>node_replayer ~'>node_tracef]
     (let [~'>node_2 (m/signal! >b)
           ~'>node_1 (m/signal! >a)
           ~'>node_0 (m/signal! (m/latest + ~'>node_1 ~'>node_2))]
       (trace! ~'>node_tracef (m/stream! (m/relieve merge (m/ap (amb= {[0] (m/?? ~'>node_0)}
                                                                      {[1] (m/?? ~'>node_1)}
                                                                      {[2] (m/?? ~'>node_2)}))))))))


(defprotocol Observable
  (subscribe! [this listenf])
  (unsubscribe! [this listenf]))

(defprotocol IReplay
  (replay! [this effect]))

(defn log! [reactor]
  (let [!trace (atom [])]
    (subscribe! reactor #(swap! !trace conj %))
    !trace))

(deftype Reactor [cancel   ;; Stop the reactor
                  !callbacks
                  !replayers]
  IReplay
  (replay! [_ frame-effects]
    (doseq [cb @!replayers]
      (cb frame-effects)))
  Observable
  (subscribe! [_ f] (swap! !callbacks conj f))
  (unsubscribe! [_ f] (swap! !callbacks disj f)))

(defn reactor! [initf]
  (let [!callbacks (atom #{})
        !replayers (atom #{})
        task       (m/reactor
                    (let [>replayer (m/stream! (m/observe (fn [cb]
                                                            (swap! !replayers conj cb)
                                                            (fn []
                                                              (swap! !replayers disj cb)))))]
                      (initf >replayer
                             (fn [effects]
                               (doseq [cb @!callbacks]
                                 (cb effects))))))
        cancel     (task (fn [_] (prn "Success"))
                         prn)]
    (->Reactor cancel !callbacks !replayers)))


(defmacro dataflow [ast & [passives]]
  `(reactor! ~(emit {:analyzed-ast (analyze ast)
                     :passives     passives})))
