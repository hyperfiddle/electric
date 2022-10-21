(ns dustin.y2020.monad-state-joinr)


(defmacro run-state [init state-symb bindings & body]
  (let [steps
        (->> (for [[l expr] (partition 2 bindings)]
               [l `(let [state# (deref ~state-symb)
                         [new-state# res#] (~(first expr) state# ~@(rest expr))
                         ~'_   (reset! ~state-symb new-state#)]
                     res#)])
          (reduce (fn [acc [l r]]
                    (conj acc l r)) []))]
    `(let [~state-symb (atom ~init)
           ~@steps]
       [(deref ~state-symb) ~@body])))


(defn random [state n]
  [state (rand-int n)])

(defn increment [state id n]
  (let [res (+ (get state id) n)]
    [(assoc state id res) res]))

(defn decrement [state id n]
  (let [res (- (get state id) n)]
    [(assoc state id res) res]))

(defn transfer [init-state from to amount]
  (run-state init-state state
    [l (decrement  from amount)
     r (increment  to amount)]
    amount))


(defn demo [init-state from to]
  (run-state init-state state
    [amount   (random 10)
     res      (transfer from to amount)]
    {:transferred res :from from :to to}))

(comment
  (demo {:a 10 :b 20} :a :b)
  := [{:a 7, :b 23} {:transferred 3, :from :a, :to :b}]
  (demo {:a 10 :b 20} :a :b)
  := [{:a 8, :b 22} {:transferred 2, :from :a, :to :b}]
  (demo {:a 10 :b 20} :a :b)
  := [{:a 10, :b 20} {:transferred 0, :from :a, :to :b}]
  (demo {:a 10 :b 20} :a :b)
  := [{:a 10, :b 20} {:transferred 0, :from :a, :to :b}]
  )
