(ns leo.extend-seq
  (:require [missionary.core :as m]
            [minitest :refer [tests]]
            [clojure.set :as set]))

(defn map-by [f xs]
  (into {} (map (juxt f identity)) xs))

;; a -> id, Flow #{a} -> Flow {id, Flow a}
;; #{{:id 0 :email "alice"}
;;   {:id 1 :email "bob"}
;;   {:id 2 :email "charlie"}} ->
;; {0 Flow{:id 0 :email "alice"}
;;  1 Flow{:id 1 :email "bob"}
;;  2 Flow{:id 2 :email "charlie"}
;; }
;; returns a discrete flow
(defn stabilize-collection-entities [kf flow]
  (->> flow
    (m/transform
      (comp (fn [rf]
              (let [prev (volatile! {})]                    ;; id -> atom
                (fn
                  ([] (rf))
                  ([r] (rf r))
                  ([r x]
                   (let [mapped (map-by kf x)
                         _ (run! (fn [[id at]]
                                   (if-some [[_ x] (find mapped id)]
                                     (when-not (= x @at) (reset! at x))
                                     (do (prn id :terminated)
                                         (vswap! prev dissoc id)))) @prev)
                         new-ids (set/difference (set (keys mapped)) (set (keys @prev)))
                         id->atom (into {} (map (fn [id] [id (atom (mapped id))])) new-ids)]
                     (vswap! prev merge id->atom)
                     (rf r (into {} (map (juxt key (comp m/watch val))) id->atom)))
                   ))))
        (remove #{{}})))))

(comment
  (def !input (atom #{{:id 0 :email "alice"}
                      {:id 1 :email "bob"}
                      {:id 2 :email "charlie"}}))

  (def >>extended (stabilize-collection-entities :id (m/watch !input)))
  (def !ext (>>extended #(prn :ext-ready) #(prn :ext-done)))
  (def flows @!ext)
  ;; ext-ready
  flows := {0 _ 1 _ 2 _}
  (def !bob ((flows 1) #(prn :bob-ready) #(prn :bob-done)))
  ;; bob-ready
  @!bob := {:id 1 :email "bob"}


  (swap! !input (fn [xs]
                  (into #{} (map (fn [x]
                                   (if (= (:id x) 1)
                                     (assoc x :email "BOB") x))) xs)))

  @!bob := {:id 1 :email "BOB"}
  @!ext

  (swap! !input (fn [xs] (into #{} (remove (comp #{0} :id)) xs)))

  @!ext

  (swap! !input conj {:id 3 :email "don"})

  (def >don (get @!ext 3))

  )

(defn diff-by [kf c1 c2]
  (let [s1 (into #{} (map kf) c1)
        s2 (into #{} (map kf) c2)]
    [(set/difference s2 s1)
     (set/difference s1 s2)]))

(tests
  (diff-by :id #{{:id 0}} #{{:id 0}}) := [#{} #{}]
  (diff-by :id #{} #{{:id 0 :email "alice"}}) := [#{0} #{}]
  (diff-by :id #{{:id 0 :email "alice"}} #{}) := [#{} #{0}]
  (diff-by :id #{{:id 0 :email "alice"}} #{{:id 1 :email "bob"}}) := [#{1} #{0}]
  (diff-by :id #{{:id 0 :email "alice"}} #{{:id 0 :email "bob"}}) := [#{} #{}]

  #_(= (patch x (diff x y)) y)                                ;; for any x and y

  )

(defn diffp [d i]
  (fn [rf]
    (let [prev (volatile! i)]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r x]
         (let [p @prev]
           (vreset! prev x)
           (rf r (d p x))))))))

(tests
  (sequence (diffp - 0) (range 1 10))
  := (list -1 -1 -1 -1 -1 -1 -1 -1 -1)

  (sequence (diffp (partial diff-by identity) #{}) [#{0 1 2} #{1 2} #{1 2 3}])
  := (list [#{0 1 2} #{}] [#{} #{0}] [#{3} #{}])

  )

(defn focus-entity
  "Focus a cont flow of successive query results into a flow of successive elements with targetted identity."
  [kf id flow]
  (->> flow
    (m/transform
         (comp
           (map (fn [xs]
                  (reduce (fn [r x]
                            (if (= id (kf x))
                              (reduced x) r))
                    ::not-found xs)))
           (remove #{::not-found})))
    (m/relieve {})))

(tests
  (def !xs (atom #{}))
  (def !s ((focus-entity :id 0 (m/watch !xs)) #(prn :ready) #(prn :done)))
  (reset! !xs #{{:id 0 :email "alice"}})
  @!s := {:id 0 :email "alice"}
  (reset! !xs #{{:id 0 :email "ALICE"}})
  @!s := {:id 0 :email "ALICE"}

  )

(defn active-flows [kf flow]
  (fn [rf]
    (let [active (volatile! {})]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r [created retracted]]
         (vreset! active (reduce dissoc @active retracted))
         (vreset! active (reduce (fn [r id]
                                   (assoc r id (focus-entity kf id flow)))
                           @active created))
         (rf r @active))))))

(comment
  (sequence (active-flows identity (m/watch (atom nil)))
    [[#{0 1 2} #{}]
     [#{} #{1}]
     [#{4} #{2}]])

  := (list
       {0 ?x, 1 ?y, 2 ?z}
       {0 ?x, 2 ?z}
       {0 ?x, 4 ?_})

  )

(defn extend-seq [kf flow]
  (->> flow (m/transform (comp (diffp (partial diff-by kf) #{}) (active-flows kf flow)))))

(tests
  (def !xs (atom #{{:db/id 1}
                   {:db/id 2}
                   {:db/id 3}}))
  (def >>xs (extend-seq :db/id (m/watch !xs)))
  ;(capI >>xs) := '[_ _ _]

  (def !flattened
    ((->> (m/ap (let [xs (m/?! >>xs)]
                  (m/?! (apply m/latest (fn [& vs] (zipmap (keys xs) vs)) (vals xs)))))
       (m/relieve {}))
     #(prn :ready) #(prn :done)))
  @!flattened := {1 #:db{:id 1}, 3 #:db{:id 3}, 2 #:db{:id 2}}

  (reset! !xs #{{:db/id 1 :email "alice"}
                {:db/id 2}
                {:db/id 3}
                {:db/id 4}})

  @!flattened := {1 {:db/id 1, :email "alice"}, 3 #:db{:id 3}, 2 #:db{:id 2}, 4 #:db{:id 4}}

  )

;; discrete extend-seq ? what is the use case ? give it another name


