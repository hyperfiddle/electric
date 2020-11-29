(ns dustin.hfql19
  "dataflow"
  (:refer-clojure :exclude [sequence])
  (:require [dustin.fiddle :refer :all]
            [dustin.hf-nav :refer :all]
            [meander.epsilon :as m :refer [match]]
            [minitest :refer [tests]]
            [hyperfiddle.fabric :as R]))


; Monad m => StateT s m b
; newtype SR = StateT (Map Sym Promise a) Promise b
(defn pureF [a]
  (fn [s] (R/pure [s a])))

(defn runS [SMa s]
  (SMa s))

;(defn runR [Rb a] (R/put Rb a)) ; nonsense

(defn bindF [SRa f]
  (fn [s]
    (R/bind (runS SRa s) (fn [[s' a]]
      (runS (f a) s')))))

(tests
  ;@(runS (pureSR 1) {}) => @(R/pure [{} 1])
  @(R/cap (runS (pureF 1) {}))
  => [{} 1]

  (let [>z (runS
             (bindF (pureF 1) (fn [a]
               (fn [s]
                 #_(R/pure [s a])
                 (R/bind (get s '>c) (fn [c]
                   (let [b (+ a c)]
                     (R/pure [s b])))))))
             {'>c (R/pure 2)})]
    (second @(R/cap >z)))
  ;=> [{'>c _} 3]
  => 3
  )

(defn fmapF [f & SRas]
  (fn [s]
    (let [Ras (->> SRas
                (map (fn [SRa]
                       (->> (runS SRa s)
                         (R/fmap (fn [[s' a]]
                                   a))))))]
      (apply R/fmap
        (fn [& as]
          [s (apply f as)])
        Ras))))

(tests
  @(R/cap (runS (fmapF + (pureF 1) (pureF 2)) {}))
  => @(R/cap (R/pure [{} 3]))
  => [{} 3]

  @(R/cap (runS (fmapF vector (pureF 1) (pureF 2)) {}))
  => [{} [1 2]]

  @(R/cap (runS (fmapF apply (pureF +) (pureF [1 2])) {}))
  => [{} 3]

  ;@(R/cap (runS (fmapF identity (pureF 17592186045421)) {}))
  )

(defn sequenceF [mas]
  (apply fmapF vector mas))

(tests
  @(R/cap (runS (sequenceF []) {}))
  => #_[{} []] nil     ; no puts happened, there are no input nodes or constants
  @(R/cap (runS (sequenceF [(pureF 1) (pureF 2)]) {}))
  => [{} [1 2]]

  @(R/cap (runS (fmapF apply (pureF +) (sequenceF [(pureF 1) (pureF 2)])) {}))
  => @(R/cap (runS (fmapF #(apply % %&) (pureF +) (pureF 1) (pureF 2)) {})) ; same thing
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
               (if-let [v (clojure.core/resolve k)] (R/pure v))
               (R/pure [::no-def-for k])                    ; allocate an input ?!
               #_(rejected k))]
      (R/bind >v (fn [v]
                   (println 'askF v)
                   ; if not error ?
                   (R/pure [scope v]))))))

(tests
  (second @(R/cap (runS (askF '>a) {'>a (R/pure 1)})))
  => 1

  (second @(R/cap (runS (askF '>a) {'>a (R/fmap identity (R/pure 1))})))
  => 1

  ;@(R/cap (R/bind (R/fmap identity (R/pure 1)) R/pure))
  ;@(R/cap (R/bind (R/pure 1) R/pure))

  (second @(R/cap (runS (askF '>a) {})))
  => [:dustin.hfql19/no-def-for '>a]
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

  @(R/cap (runS (hf-apply :dustingetz/gender 17592186045421) {}))
  => [{} :dustingetz/male]

  ;@(R/cap (runS (hf-apply (:dustingetz/gender '>b) 17592186045421) {'>b (R/pure 17592186045421)}))
  ;=> [{} :dustingetz/male]

  (second @(R/cap (runS (hf-apply '(inc >a) 17592186045421) {'>a (R/pure 1)})))
  => 2

  (second @(R/cap (runS (hf-apply '(inc >a) 17592186045421) {'>a (R/fmap identity (R/pure 1))})))
  => 2

  @(R/cap (R/fmap identity (R/pure 1))) => 1)

(defn hf-eval [edge Fa]
  (bindF Fa (fn [a]
  (bindF (hf-apply edge a) (fn [b]
  (fn [s] (R/pure [(assoc s (hf-edge->sym edge) (R/pure b)) b])))))))

(tests
  (second @(R/cap (runS (hf-eval :dustingetz/gender (pureF 17592186045421)) {})))
  => :dustingetz/male
  => (second @(R/cap (runS (hf-eval :dustingetz/gender (fmapF identity (pureF 17592186045421))) {})))

  )

(defn hf-pull-F [pat Fa]
  (match pat
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

(defn Ra->Fa                                               ; lift ?
  "upgrade base monad value into transformed monad value"
  [Ra]
  (fn [s]
    (R/fmap
      (fn [a] [s a])
      Ra)))

(tests
  ;(Ra->SRa (p/resolved 1)) => (fn [s] (p/resolved [s 1]))
  @(R/cap (runS (Ra->Fa (R/pure 1)) {}))
  => @(R/cap (runS (fn [s] (R/pure [s 1])) {}))
  => @(R/cap (R/pure [{} 1]))
  => [{} 1]

  (.. (runS (Ra->Fa (R/pure 17592186045421)) {}) -node -val)
  (.. (R/pure 17592186045421) -node -val)

  @(R/cap (runS (Ra->Fa (R/pure 17592186045421)) {}))
  @(R/cap (runS (pureF 17592186045421) {}))
  )

(defn hf-pull [pat Ra & [s]]
  (R/fmap
    (fn [[s' b]] #_(R/pure) b)                              ; extract
    (runS (hf-pull-F pat (Ra->Fa Ra)) (or s {}))))

(tests
  @(R/cap (runS (hf-pull-F :dustingetz/gender
                  #_(pureF 17592186045421)
                  (fmapF identity (pureF 17592186045421))
                  #_(Ra->Fa (R/pure 17592186045421))) {}))
  => [{} #:dustingetz{:gender :dustingetz/male}]

  (R/cap (hf-pull :dustingetz/gender (R/pure 17592186045421)))
  => #:dustingetz{:gender :dustingetz/male}

  @(R/cap (hf-pull :db/ident (R/pure 17592186045421)))
  => #:db{:ident :dustingetz/mens-small}

  @(R/cap (hf-pull [:db/ident] (R/pure 17592186045421)))
  => #:db{:ident :dustingetz/mens-small}

  @(R/cap (hf-pull [:db/ident :db/id] (R/pure 17592186045421)))
  => #:db{:ident :dustingetz/mens-small :id 17592186045421}

  ;@(hf-pull '[{:dustingetz/gender [:db/ident :db/id]}] (p/resolved 17592186045421))
  ;=> #:dustingetz{:gender #:db{:ident :dustingetz/male :id 17592186045418}}
  ;
  ;@(hf-pull '[(shirt-size dustingetz/gender)] (p/resolved nil)
  ;   {'dustingetz/gender (resolved :dustingetz/male)})
  ;=> {'(shirt-size dustingetz/gender) 17592186045421}
  ;
  ;@(hf-pull '[{:dustingetz/gender [(shirt-size dustingetz/gender)]}]
  ;   (p/resolved 17592186045421))
  ;=> #:dustingetz{:gender {'(shirt-size dustingetz/gender) 17592186045421}}
  ;
  ;@(hf-pull '[{:dustingetz/gender
  ;             [{(shirt-sizes dustingetz/gender) [:db/ident]}]}]
  ;   (p/resolved 17592186045421))
  ;=> '#:dustingetz{:gender {(shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
  ;                                                           #:db{:ident :dustingetz/mens-medium}
  ;                                                           #:db{:ident :dustingetz/mens-large}]}}
  ;
  ;@(hf-pull
  ;   '[:db/id
  ;     :dustingetz/type
  ;     {:dustingetz/gender
  ;      [{(shirt-sizes dustingetz/gender >needle)
  ;        [:db/ident
  ;         :db/id]}]}]
  ;   (p/resolved 17592186045421)
  ;   {'>needle (resolved "")})
  ;=> @(p/resolved
  ;      '{:db/id 17592186045421,
  ;        :dustingetz/type :dustingetz/shirt-size,
  ;        :dustingetz/gender
  ;        {(shirt-sizes dustingetz/gender >needle)
  ;         [#:db{:ident :dustingetz/mens-small, :id 17592186045421}
  ;          #:db{:ident :dustingetz/mens-medium, :id 17592186045422}
  ;          #:db{:ident :dustingetz/mens-large, :id 17592186045423}]}})
  ;
  ;@(hf-pull
  ;   '[{(submissions)
  ;      [:dustingetz/email]}]
  ;   (p/resolved nil)
  ;   {'needle (resolved "")})
  ;=> '{(submissions) [#:dustingetz{:email "alice@example.com"}
  ;                    #:dustingetz{:email "bob@example.com"}
  ;                    #:dustingetz{:email "charlie@example.com"}]}
  ;
  ;@(hf-pull
  ;   '[{(submissions needle)
  ;      [:dustingetz/email]}]
  ;   (p/resolved nil)
  ;   {'needle (resolved "bob")})
  ;=> '{(submissions needle) [#:dustingetz{:email "bob@example.com"}]}
  ;
  ;@(hf-pull
  ;   '[{(submissions needle)
  ;      [[(identity) :as submission]
  ;       :dustingetz/email
  ;       {:dustingetz/gender
  ;        [:db/ident
  ;         :db/id
  ;         {(shirt-sizes dustingetz/gender needle:submission)
  ;          [:db/ident]}]}
  ;       :db/id]}
  ;     {(genders)
  ;      [:db/ident :db/id]}]
  ;
  ;   (p/resolved nil))
  ;=> {'>first (Node "charlie")
  ;    '>second (Node nil)
  ;    '>last (Node nil)
  ;    >(user.hello-world/submission-master >first) (Node '())
  ;    > (submission-detail %) (Node 419)
  ;    ;'>email1:418 (Node nil)
  ;    ;'>shirt-size-needle:418 (Node nil)
  ;    }
  ;
  ;=> '{(submissions needle) [{:dustingetz/email  "alice@example.com",
  ;                            :dustingetz/gender {:db/ident :dustingetz/female,
  ;                                                :db/id    17592186045419,
  ;                                                (shirt-sizes dustingetz/gender needle2)
  ;                                                          [#:db{:ident :dustingetz/womens-medium}
  ;                                                           #:db{:ident :dustingetz/womens-large}
  ;                                                           #:db{:ident :dustingetz/womens-small}]},
  ;                            :db/id             17592186045428}
  ;                           {:dustingetz/email  "charlie@example.com",
  ;                            :dustingetz/gender {:db/ident :dustingetz/male,
  ;                                                :db/id    17592186045418,
  ;                                                (shirt-sizes dustingetz/gender needle2)
  ;                                                          [#:db{:ident :dustingetz/mens-small}
  ;                                                           #:db{:ident :dustingetz/mens-medium}
  ;                                                           #:db{:ident :dustingetz/mens-large}]},
  ;                            :db/id             17592186045430}],
  ;     (genders)            [#:db{:ident :dustingetz/male, :id 17592186045418}
  ;                           #:db{:ident :dustingetz/female, :id 17592186045419}]}

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
