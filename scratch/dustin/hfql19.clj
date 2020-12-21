(ns dustin.hfql19
  "missionary"
  (:refer-clojure :exclude [sequence])
  (:require [dustin.fiddle :refer :all]
            [dustin.hf-nav :refer :all]
            [meander.epsilon :as m]
            [minitest :refer [tests]]
            [missionary.core :refer [latest relieve watch ap ?!]]))

(defn bindI [>a f] (relieve {} (ap (?! (f (?! >a))))))
(defn pureI [a] (watch (atom a)))
(defn fmapI [f & >as] (apply latest f >as))

; Monad m => StateT s m b
; newtype SR = StateT (Map Sym Promise a) Promise b
(defn pureF [a]
  (fn [s] (pureI [s a])))

(defn runS [SMa s]
  (SMa s))

;(defn runR [Rb a] (R/put Rb a)) ; nonsense

(defn bindF [SIa f]
  (fn [s]
    (bindI (runS SIa s) (fn [[s' a]]
      (runS (f a) s')))))

(tests
  !! (def >z (runS (pureF 1) {}))
  !! (def !z (>z #(println :ready) #(println :done)))
  @!z => [{} 1]

  @((runS (pureF 1) {}) #() #())
  => [{} 1]

  (let [>z (runS
             (bindF (pureF 1) (fn [a]
               (fn [s]
                 #_(R/pure [s a])
                 (bindI (get s '>c) (fn [c]
                   (let [b (+ a c)]
                     (pureI [s b])))))))
             {'>c (pureI 2)})]

    (second @(>z #() #())))
  ;=> [{'>c _} 3]
  => 3
  )

(defn fmapF [f & SIas]
  (fn [s]
    (let [Ias (->> SIas
                (map (fn [SIa]
                       (->> (runS SIa s)
                         (fmapI (fn [[s' a]]
                                   a))))))]
      (apply fmapI
        (fn [& as]
          [s (apply f as)])
        Ias))))

(tests
  @((runS (fmapF + (pureF 1) (pureF 2)) {}) #() #())
  => [{} 3]

  @((runS (fmapF vector (pureF 1) (pureF 2)) {}) #() #())
  => [{} [1 2]]

  @((runS (fmapF apply (pureF +) (pureF [1 2])) {}) #() #())
  => [{} 3]

  ;@(R/cap (runS (fmapF identity (pureF 17592186045421)) {}))
  )

(defn sequenceF [mas]
  (apply fmapF vector mas))

(tests
  @((runS (sequenceF []) {}) #() #())
  => [{} []]
  @((runS (sequenceF [(pureF 1) (pureF 2)]) {}) #() #())
  => [{} [1 2]]

  @((runS (fmapF apply (pureF +) (sequenceF [(pureF 1) (pureF 2)])) {}) #() #())
  => @((runS (fmapF #(apply % %&) (pureF +) (pureF 1) (pureF 2)) {}) #() #())
  => [{} 3]
  )

; ---

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(tests
  (hf-edge->sym :dustingetz/gender) => 'dustingetz/gender
  (hf-edge->sym '>a) => '>a
  )

(defn askF [k]
  (fn [scope]
    (let [>v (or (get scope k)
               (if-let [v (clojure.core/resolve k)] (pureI v))
               (pureI [::no-def-for k])                     ; allocate an input ?!
               #_(rejected k))]
      (bindI >v (fn [v]
                  (println 'askF v)
                  ; if not error ?
                  (pureI [scope v]))))))

(tests
  (second @((runS (askF '>a) {'>a (pureI 1)}) #() #()))
  => 1

  (second @((runS (askF '>a) {'>a (fmapI identity (pureI 1))}) #() #()))
  => 1

  ;@(R/cap (R/bind (R/fmap identity (R/pure 1)) R/pure))
  ;@(R/cap (R/bind (R/pure 1) R/pure))

  (second @((runS (askF '>a) {}) #() #()))
  => [::no-def-for '>a]
  )

;(defn setSR [k v]
;  (fn [s] (resolved [(assoc s k v) nil])))

(defn hf-apply [edge a]
  (cond
    (keyword? edge) (pureF (hf-nav edge a))
    (seq? edge) (let [[f & args] edge]
                  (fmapF apply (askF f) (sequenceF (map askF args))))
    () (do (println "hf-eval unmatched edge: " edge)
           (pureF [::unmatched-edge edge]))))

(tests
  (hf-nav :dustingetz/gender 17592186045421) => :dustingetz/male

  @((runS (hf-apply :dustingetz/gender 17592186045421) {}) #() #())
  => [{} :dustingetz/male]

  ;@(R/cap (runS (hf-apply (:dustingetz/gender '>b) 17592186045421) {'>b (R/pure 17592186045421)}))
  ;=> [{} :dustingetz/male]

  (second @((runS (hf-apply '(inc >a) 17592186045421) {'>a (pureI 1)}) #() #()))
  => 2

  (second @((runS (hf-apply '(inc >a) 17592186045421) {'>a (fmapI identity (pureI 1))}) #() #()))
  => 2

  @((fmapI identity (pureI 1)) #() #()) => 1)

(defn hf-eval [edge Fa]
  (bindF Fa (fn [a]
  (bindF (hf-apply edge a) (fn [b]
  (fn [s] (pureI [(assoc s (hf-edge->sym edge) (pureI b)) b])))))))

(tests
  (second @((runS (hf-eval :dustingetz/gender (pureF 17592186045421)) {}) #() #()))
  => :dustingetz/male
  => (second @((runS (hf-eval :dustingetz/gender (fmapF identity (pureF 17592186045421))) {}) #() #()))
  )

(defn hf-pull-F [pat Fa]
  (m/match pat
    [!pats ...]
    (as-> Fa Fa
      (sequenceF (map (fn [pat] (hf-pull-F pat Fa)) !pats))
      (fmapF (fn [as] (apply merge as)) Fa))

    {& (m/seqable [?edge ?pat])}
    (as-> Fa Fa
      (hf-eval ?edge Fa)
      (bindF Fa (fn [a]
                    (if (vector? a)                         ; :db.cardinality/many
                      (sequenceF (->> a (map #(hf-pull-F ?pat (pureF %)))))
                      (hf-pull-F ?pat (pureF a)))))
      (fmapF (fn [a] {?edge a}) Fa))

    ?leaf
    (as-> Fa Fa
      (hf-eval ?leaf Fa)
      (fmapF (fn [a] {?leaf a}) Fa))))

; instance MonadTrans (StateT s) where
;   lift c = StateT $ \s -> c >>= (\x -> return (x,s))

(defn I->F
  "lift base monad value into transformed monad value"
  [Ra]
  (fn [s]
    (fmapI
      (fn [a] [s a])
      Ra)))

(tests
  ;(Ra->SRa (p/resolved 1)) => (fn [s] (p/resolved [s 1]))
  @((runS (I->F (pureI 1)) {}) #() #())
  => @((runS (fn [s] (pureI [s 1])) {}) #() #())
  => @((pureI [{} 1]) #() #())
  => [{} 1]

  @((runS (I->F (pureI 17592186045421)) {}) #() #())
  => @((runS (pureF 17592186045421) {}) #() #())
  )

(defn hf-pull [pat & [Ia s]]
  (let [s (or s {})
        Ia (or Ia (pureI nil))]
    (fmapI
      (fn [[s' b]] #_(pureI) b)                             ; extract
      (runS (hf-pull-F pat (I->F Ia)) s))))

(tests
  @((runS (hf-pull-F :dustingetz/gender
            #_(pureF 17592186045421)
            (fmapF identity (pureF 17592186045421))
            #_(I->F (R/pure 17592186045421))) {}) #() #())
  => [{} #:dustingetz{:gender :dustingetz/male}]

  @((hf-pull :dustingetz/gender (pureI 17592186045421)) #() #())
  => #:dustingetz{:gender :dustingetz/male}

  @((hf-pull :db/ident (pureI 17592186045421)) #() #())
  => #:db{:ident :dustingetz/mens-small}

  @((hf-pull [:db/ident] (pureI 17592186045421)) #() #())
  => #:db{:ident :dustingetz/mens-small}

  @((hf-pull [:db/ident :db/id] (pureI 17592186045421)) #() #())
  => #:db{:ident :dustingetz/mens-small :id 17592186045421}

  @((hf-pull [{:dustingetz/gender [:db/ident :db/id]}] (pureI 17592186045421)) #() #())
  => #:dustingetz{:gender #:db{:ident :dustingetz/male, :id 17592186045418}}

  @((hf-pull '[(shirt-size dustingetz/gender)] (pureI nil)
      {'dustingetz/gender (pureI :dustingetz/male)}) #() #())
  => {'(shirt-size dustingetz/gender) 17592186045421}

  @((hf-pull '[{:dustingetz/gender [(shirt-size dustingetz/gender)]}]
      (pureI 17592186045421)) #() #())
  => #:dustingetz{:gender {'(shirt-size dustingetz/gender) 17592186045421}}

  @((hf-pull '[{:dustingetz/gender
                [{(shirt-sizes dustingetz/gender) [:db/ident]}]}]
      (pureI 17592186045421)) #() #())
  => '#:dustingetz{:gender {(shirt-sizes dustingetz/gender)
                            [#:db{:ident :dustingetz/mens-small}
                             #:db{:ident :dustingetz/mens-medium}
                             #:db{:ident :dustingetz/mens-large}]}}

  @((hf-pull
      '[:db/id
        :dustingetz/type
        {:dustingetz/gender
         [{(shirt-sizes dustingetz/gender >needle)
           [:db/ident
            :db/id]}]}]
      (pureI 17592186045421)
      {'>needle (pureI "")}) #() #())
  => @((pureI
         '{:db/id           17592186045421,
           :dustingetz/type :dustingetz/shirt-size,
           :dustingetz/gender
                            {(shirt-sizes dustingetz/gender >needle)
                             [#:db{:ident :dustingetz/mens-small, :id 17592186045421}
                              #:db{:ident :dustingetz/mens-medium, :id 17592186045422}
                              #:db{:ident :dustingetz/mens-large, :id 17592186045423}]}})
       #() #())

  @((hf-pull
      '[{(submissions)
         [:dustingetz/email]}]
      (pureI nil)
      {'needle (pureI "")}) #() #())
  => '{(submissions) [#:dustingetz{:email "alice@example.com"}
                      #:dustingetz{:email "bob@example.com"}
                      #:dustingetz{:email "charlie@example.com"}]}

  !! (def !needle (atom nil))
  !! (def >z (hf-pull
               '[{(submissions needle)
                  [:dustingetz/email]}]
               (pureI nil)
               {'needle (watch !needle)}))

  !! (def !z (>z #(println :ready) #(println :done)))
  @!z => '{(submissions needle) [#:dustingetz{:email "alice@example.com"}
                                 #:dustingetz{:email "bob@example.com"}
                                 #:dustingetz{:email "charlie@example.com"}]}

  !! (reset! !needle "alice")
  @!z => '{(submissions needle) [#:dustingetz{:email "alice@example.com"}]}

  !! (reset! !needle "bob")
  !! (reset! !needle "zetta")
  !! (reset! !needle "e@example.com")
  @!z => '{(submissions needle) [#:dustingetz{:email "alice@example.com"}
                                 #:dustingetz{:email "charlie@example.com"}]}


  @((hf-pull
      '[{(submissions needle)
         [:dustingetz/email
          {:dustingetz/gender
           [:db/ident
            :db/id
            {(shirt-sizes dustingetz/gender #_needle:submission)              ; TODO
             [:db/ident]}]}
          :db/id]}
        {(genders)
         [:db/ident :db/id]}]

      (pureI nil)
      {'needle (pureI "e@")})
    #() #())

  => '{(submissions needle) [{:dustingetz/email  "alice@example.com",
                              :dustingetz/gender {:db/ident :dustingetz/female,
                                                  :db/id    17592186045419,
                                                  (shirt-sizes dustingetz/gender)
                                                            [#:db{:ident :dustingetz/womens-medium}
                                                             #:db{:ident :dustingetz/womens-large}
                                                             #:db{:ident :dustingetz/womens-small}]},
                              :db/id             17592186045428}
                             {:dustingetz/email  "charlie@example.com",
                              :dustingetz/gender {:db/ident :dustingetz/male,
                                                  :db/id    17592186045418,
                                                  (shirt-sizes dustingetz/gender)
                                                            [#:db{:ident :dustingetz/mens-small}
                                                             #:db{:ident :dustingetz/mens-medium}
                                                             #:db{:ident :dustingetz/mens-large}]},
                              :db/id             17592186045430}],
       (genders)            [#:db{:ident :dustingetz/male, :id 17592186045418}
                             #:db{:ident :dustingetz/female, :id 17592186045419}]}

  ;@(hf-pull
  ;   '{:dustingetz/gender
  ;     {(p/map (fn [needle]                                 ; fused effect
  ;               (shirt-size dustingetz/gender needle))
  ;        >needle)
  ;      :db/ident}}
  ;   (p/resolved 17592186045421)
  ;   {'>needle (resolved "large")})

  ;(p/map (fn [needle]                                       ; Wrong, too loose reaction
  ;         @(hf-pull
  ;            '{:dustingetz/gender
  ;              {(shirt-size dustingetz/gender needle)
  ;               :db/ident}}
  ;            (p/resolved 17592186045421)
  ;            {'needle needle}))
  ;  (resolved "large"))

  )
