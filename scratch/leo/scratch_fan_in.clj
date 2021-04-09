(ns leo.scratch-fan-in)

(comment
  (defn diff [- init]
    (fn [rf]
      (let [p (volatile! init)]
        (fn
          ([] (rf))
          ([r] (rf r))
          ([r x]
           (let [r (rf r (- x @p))]
             (vreset! p x) r))))))

  (require '[clojure.set :as set])

  ;; Flow[Set[A]] -> Flow[Diff[A]]
  (defn diff-set [>xs]
    (m/transform (diff (fn [x y]
                         [(set/difference x y)
                          (set/difference y x)])
                   #{}) >xs))

  (->> (m/enumerate [#{1 2 3} #{1 2 4} #{2}])
    (diff-set)
    (m/aggregate conj)
    (m/?))
  :=
  [[#{1 2 3} #{}]
   [#{4} #{3}]
   [#{} #{1 4}]]

  (defn inc-after [delay x]
    (->> (m/ap (prn :started x)
           (inc (m/? (m/sleep delay x))))
      (m/integrate {} x)
      (m/relieve {})))

  (def >input (atom #{1 2 3}))
  (def fi (->> (m/watch >input)
            (diff-set)
            (m/transform (mapcat (fn [[adds _rets]]
                                   (map (fn [id] (inc-after (rand-int 1000) id)) adds))))
            (fan-in (fn [x y] (prn x y) (+ x y)) 0)))
  (def it (fi #(prn :ready) #(prn :done)))

  (reset! >input #{1 2 3 4})

  ;; should spawn all 3 flows, only the first one is started
  @it
  )

(comment
  ;; dustin-lang
  (def !result (atom []))
  (def !db (atom [])) (def >db (m/watch !db))
  (defn App [>db >e]
    `(let [>xs' (hf/q'. '[:in $ :find ?e :where [?e :person/name]] >db)] ; prime means derivative
       (div.
         (pre. (pr-str @(m/integrate ... >xs')))            ; [101 102 104 106]
         (for. [>x >xs']                                    ; differential for
           (let [>ks' (entity-ks. >db >x)]
             (pre.
               (pr-str @(entity-get. >db >x ':person/name))
               (for. [>k >ks']
                 (pr-str @(entity-get. >db >x >k)))))))         ; (entity-get. '1001 ':person/name)
       ))
  (boot (Eval-incr {}) $el
    `(println. (App. '>db)))
  )

(comment
  ;; leo-lang
  (def !result (atom []))
  (def !db (atom db-value))

  ;; Query[A] -> Flow[DB] -> Flow[Diff]
  (defn q' [query >db])

  ;; ' suffix means diffs

  ;; (A -> Flow[B]) -> Flow[List[A]] -> Flow[List[B]]
  (defn reactive-for [f >xs]
    (comment TODO))

  (defmacro rfor [[s e] & body]
    `@(reactive-for (fn [~s] ~@body) (unquote ~e)))

  ;; returns differential list of keys
  (defn entity-ks [>db >eid])

  ;; returns successive values of attribute key for entity id
  (defn entity-get [>db >eid >key])

  (defn patch [])

  (def query '[:in $ :find ?e :where [?e :person/name]])

  (defn app [>db]
    (dataflow
      (let [xs' @(q' query >db)]
        [:div
         [:pre (pr-str @(m/integrate patch ~xs'))]
         (rfor [x xs']
           (let [ks' @(entity-ks >db x)]
             [:pre (pr-str @(entity-get >db ~x ~:person/name))]
             (rfor [k ks']
               (pr-str @(entity-get >db ~x ~k)))))])))

  )