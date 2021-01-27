(ns dustin.trace33
  (:require [dustin.trace17 :as trace]
            [minitest :refer [tests]]
            [missionary.core :as m]))

;;;;;;;;;;;;;
;; ANALYSE ;;
;;;;;;;;;;;;;

(def iterate-ast (partial tree-seq coll? identity))
(defn form-type [t form] (and (coll? form) (= t (first form))))
(def fmap? (partial form-type 'fmap))
(def bind? (partial form-type 'bind))

(defn peek-next [n xs] (second (nth xs n)))

(def conjv (fnil conj []))

(defn analyse
  ([ast]
   (analyse {} () (map-indexed vector (iterate-ast ast))))
  ([acc stack forms]
   (if-let [[[idx form] & rest] forms]
     (cond
       (fmap? form) (analyse (assoc acc idx {:type 'fmap
                                             :form form
                                             :f    (peek-next 1 rest)})
                      (conj stack idx)
                      (nnext rest))
       ;; (bind? form) ()
       (symbol? form) (analyse (-> (assoc acc idx {:type   'user
                                                   :form   form
                                                   :parent (peek stack)})
                                 (update-in [(peek stack) :children] conjv idx))
                        stack
                        rest)
       )
     (vec (sort-by first acc)))))

(tests
 (analyse '(fmap + >a >b))
 :=
 '[[0 {:type     fmap,
       :form     (fmap + >a >b),
       :f        +
       :children [3 4]}],
   [3 {:type   user
       :form   >a,
       :parent 0}],
   [4 {:type   user
       :form   >b,
       :parent 0}]])

;;;;;;;;;;;;;
;; EMITTER ;;
;;;;;;;;;;;;;

(defn prefixer [prefix index]
  (symbol (str prefix "_" index)))

(defmacro amb= [& forms]
  `(case (m/?= (m/enumerate (range ~(count forms))))
     ~@(interleave (range) forms)))

(defn gen-trace-pairs [prefixf analyzed-ast]
  (for [[idx _] analyzed-ast]
    `{[~idx] (m/?? ~(prefixf idx))}))

(tests
 (gen-trace-pairs (partial prefixer '>node)
                  '[[0 _]
                    [1 _]])
 := [{[0] `(m/?? ~'>node_0)}
     {[1] `(m/?? ~'>node_1)}])

(defn trace! [tracef >effects]
  (m/stream! (m/ap (tracef (m/?? >effects)))))

(defn gen-trace [prefixf analyzed-ast]
  `(trace! ~(prefixf 'tracef)
           (m/stream! (m/relieve merge (m/ap (amb= ~@(gen-trace-pairs prefixf analyzed-ast)))))))

(defn emit-bindings [prefixf analyzed-ast passives]
  (for [[idx {:keys [type form f children]}] (reverse analyzed-ast)]
    (if (contains? passives idx)
      `[~(prefixf idx) (from-trace! [~idx] ~(prefixf 'replayer))]
      (case type
        fmap `[~(prefixf idx) (m/signal! (m/latest ~f ~@(map prefixf children)))]
        user `[~(prefixf idx) (m/signal! ~form)]))))

(defn emit [{:keys [analyzed-ast prefix passives]
             :or   {prefix (gensym)}}]
  (let [prefixf  (partial prefixer prefix)
        bindings (mapcat identity (emit-bindings prefixf analyzed-ast passives))]
    `(fn ~(mapv prefixf ['replayer 'tracef])
       (let [~@bindings]
         ~(gen-trace prefixf analyzed-ast)))))

(tests
 (emit {:analyzed-ast (analyse '(fmap clojure.core/+ >a >b))
        :prefix       '>node
        :passives     #{3 4}})
 :=
 `(fn [~'>node_replayer ~'>node_tracef]
    (let [~'>node_4 (from-trace! [4] ~'>node_replayer)
          ~'>node_3 (from-trace! [3] ~'>node_replayer)
          ~'>node_0 (m/signal! (m/latest + ~'>node_3 ~'>node_4))]
      (trace! ~'>node_tracef (m/stream! (m/relieve merge (m/ap (amb= {[0] (m/?? ~'>node_0)}
                                                                     {[3] (m/?? ~'>node_3)}
                                                                     {[4] (m/?? ~'>node_4)}))))))))

(tests
 (emit {:analyzed-ast (analyse `(~'fmap + >a >b))
        :prefix       '>node})
 :=
 `(fn [~'>node_replayer ~'>node_tracef]
    (let [~'>node_4 (m/signal! >b)
          ~'>node_3 (m/signal! >a)
          ~'>node_0 (m/signal! (m/latest + ~'>node_3 ~'>node_4))]
      (trace! ~'>node_tracef (m/stream! (m/relieve merge (m/ap (amb= {[0] (m/?? ~'>node_0)}
                                                                     {[3] (m/?? ~'>node_3)}
                                                                     {[4] (m/?? ~'>node_4)}))))))))


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
                     (initf !replayers
                       (fn [effects]
                         (doseq [cb @!callbacks]
                           (cb effects)))))
        cancel     (task (fn [_] (prn "Success"))
                     prn)]
    (->Reactor cancel !callbacks !replayers)))


(defmacro dataflow [ast & [passives]]
  `(reactor! ~(emit {:analyzed-ast (analyse ast)
                     :passives     passives})))

(tests
  "simple diamond"

  (def !a (atom 1))
  (def !b (atom 1))

  (def >a (m/watch !a))
  (def >b (m/watch !b))

  (def ast '(fmap + >a >b))

  ; this is a source map

  (tree-seq coll? identity ast) := '((fmap + >a >b) fmap + >a >b)
  (nth (tree-seq coll? identity ast) 0) := '(fmap + >a >b)
  (nth (tree-seq coll? identity ast) 1) := 'fmap
  (nth (tree-seq coll? identity ast) 2) := '+
  (nth (tree-seq coll? identity ast) 3) := '>a
  (nth (tree-seq coll? identity ast) 4) := '>b

  ; there is not always a symbol, there are anonymous nodes
  (def d (dataflow (fmap + >a >b)))



  (def !trace (log! d))

  (swap! !b inc)

  @!trace
  := [{[4] 2
       [0] 3}]

  (reset! !a 10)
  @!trace
  := [{[4] 2
       [0] 3}
      {[3] 10
       [0] 12}])
