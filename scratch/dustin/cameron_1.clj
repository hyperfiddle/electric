(ns dustin.cameron_1)

; dataflow monad

; ui programming

(def >a)
(def >z (via (->Fabric)
          (let [b ~(inc ~>a)
                c (try ~(dec ~>a)
                       (catch ...))]
            (return
              (vector b c :x))
            #_[~>b ~>c :x])))

(bind (fmap inc >a) (fn [a]
                      (bind (fmap dec >a) (fn [b]
                                            (return (vector b c :x))))))


(! :Clj.try ...)



(let [b (inc a)
      c (dec a)]
  (! :Writer.trace ...)
  (trace ...)

  (return
    (vector b c :x))
  #_[~>b ~>c :x])


(def xs
  [{::name "Alice" ::gender ::female}
   {::name "Bob" ::gender ::male}])

[:ol
 (for [x xs]
   [:li (::name x)])]


(def xs (df/input))
(def >dom (via ...
            [:ol
             (for [x ~xs]
               [:li (::name ~x)])]))

(M (M x))

(df/put xs [{::name "Alice" ::gender ::female}
            {::name "Bob" ::gender ::male}])

(df/on >dom println)                                        ; send to react



; remap clojure semantics to free effects

;