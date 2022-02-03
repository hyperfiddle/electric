(defnode persons [db needle & [sort-order]]
  (sort (or sort-order <)
        (datomic.api/q '[:find [?e ...] :in $ ?needle :where
                         [?e :person/email ?email]
                         [(clojure.string/includes? ?email ?needle)]]
                       db (or needle ""))))

(defnode render-persons [db]
  ~@(dom/div
      (dom/h1 "submissions")
      (let [needle (dom/input)]
        (dom/table
          ~@(photon/for [e (persons db needle)]
              (let [{:keys [:db/id
                            :person/email
                            :person/gender
                            :person/shirt-size]} (datomic.api/entity db e)]
                ~@(dom/tr
                    (dom/td (str id))
                    (dom/td email)
                    (dom/td (pr-str gender))
                    (dom/td (pr-str shirt-size)))))))))


(defnode f [x]
  (println 'before)
  x
  (println 'after))

(photon/main
  (f (println 'inc)))

; there are two kinds of laziness in flows
; 1.
; 2. once the value is bound, it is pulled when someone needs it.
; 2 this is lazy sampling in missionary docs



(comment
  ;; lazy seqs are lazy and memoized
  (def l (map prn (range 10)))
  (seq l)

  ;; eductions are lazy and not memoized
  (def e (eduction (map prn) (range 10)))
  (seq e)
  (defn f [] (* 6 7))
  (def g (memoize f))
  (f)
  (g)
  (def !input (atom [1 2 3]))
  (defnode bar dec)
  (defnode foo []
    (bind [bar inc]
          (p/for [x ~(m/watch !input)]
                 (bar x))))
  ;; first result : (2 3 4)
  (swap! !input conj 4)
  ;; second result : (2 3 4 5)
  (def ^:dynamic *bar* dec)
  (binding [*bar* inc]
    (doall (for [x [1 2 3]]
             (*bar* x))))
  ;; returns (0 1 2), we agree it's broken
  ;; L: dynamic vars are broken
  ;; D,G: make lazy things in an eager lang is broken
  (binding [*bar* inc]
    (for [x [1 2 3]]
      (*bar* x)))
  )