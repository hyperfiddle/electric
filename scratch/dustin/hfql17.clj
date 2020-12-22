(ns dustin.hfql17
  (:refer-clojure :exclude [sequence])
  (:require [dustin.fiddle :refer :all]
            [dustin.hf-nav :refer :all]
            [meander.epsilon :as m :refer [match]]
            [minitest :refer [tests]]
            [promesa.core :as p :refer [then resolved rejected]]))


; Monad m => StateT s m b
; newtype SR = StateT (Map Sym Promise a) Promise b
(defn pureSR [a]
  (fn [s] (resolved [s a])))

(defn runS [SMa s]
  (SMa s))

(defn bindSR [SRa f]
  (fn [s]
    (then (runS SRa s) (fn [[s' a]]
      (runS (f a) s')))))

(tests
  ;(runStateT (pureSR 1) {}) := (p/resolved [{} 1]) ; fails
  @(runS (pureSR 1) {}) := @(resolved [{} 1])

  (->
    (runS
      (bindSR (pureSR 1) (fn [a]
        (fn [s]
          (then (get s '>c) (fn [c]
            (let [b (+ a c)]
              [s b]))))))
      {'>c (resolved 2)})
    deref second)
  := 3
  )

;(defn fmapS [f & Sas]
;  (fn [s]
;    (let [as (->> Sas
;               (map (fn [Sa]
;                      (let [[s' a] (runS Sa s)]
;                        a))))]
;      [s (apply f as)])))

(defn fmapSR [f & SRas]
  (fn [s]
    (let [Ras (->> SRas
               (map (fn [SRa]
                      (->> (runS SRa s)
                        (p/map (fn [[s' a]]
                                 a))))))]
      (p/map
        (fn [as]
          [s (apply f as)])
        (p/all Ras)))))

(tests
  @(runS (fmapSR + (pureSR 1) (pureSR 2)) {})
  := @(resolved [{} 3])

  @(runS (fmapSR vector (pureSR 1) (pureSR 2)) {})
  := [{} [1 2]]

  @(runS (fmapSR apply (pureSR +) (pureSR [1 2])) {})
  := [{} 3]
  )

(defn sequenceSR [mas]
  (apply fmapSR vector mas))

(tests
  @(runS (sequenceSR []) {})
  := [{} []]
  @(runS (sequenceSR [(pureSR 1) (pureSR 2)]) {})
  := [{} [1 2]]

  @(runS (fmapSR apply (pureSR +) (sequenceSR [(pureSR 1) (pureSR 2)])) {})
  := @(runS (fmapSR #(apply % %&) (pureSR +) (pureSR 1) (pureSR 2)) {}) ; same thing
  := [{} 3]
  )

; ---

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(defn askSR [k]
  (fn [scope]
    (let [>v (or (get scope k)
               (if-let [v (clojure.core/resolve k)] (resolved v))
               (rejected k))]
      (then >v
        (fn [v] [scope v])))))

;(defn setSR [k v]
;  (fn [s] (resolved [(assoc s k v) nil])))

(defn hf-apply [edge a]
  (cond
    (keyword? edge) (pureSR (hf-nav edge a))
    (seq? edge) (let [[f & args] edge]
                  (fmapSR apply (askSR f) (sequenceSR (map askSR args))))
    () (do (println "hf-eval unmatched edge: " edge)
           (pureSR nil))))

(defn hf-eval [edge SRa]
  (bindSR SRa (fn [a]
  (bindSR (hf-apply edge a) (fn [b]
  (fn [s] (resolved [(assoc s (hf-edge->sym edge) (resolved b)) b])))))))

(defn hf-pull-SR [pat SRa]
  (match pat
    {& (m/seqable [?edge ?pat])}                            ; one
    (as-> SRa SRa
      (hf-eval ?edge SRa)
      (hf-pull-SR ?pat SRa)
      #_(bindSR SRa (fn [a] (hf-pull-SR ?pat a)))
      (fmapSR (fn [a] {?edge a}) SRa))

    ?leaf
    (as-> SRa SRa
      (hf-eval ?leaf SRa)
      (fmapSR (fn [a] {?leaf a}) SRa))))

; instance MonadTrans (StateT s) where
;   lift c = StateT $ \s -> c >>= (\x -> return (x,s))

(defn Ra->SRa                                               ; lift ?
  "upgrade base monad value into transformed monad value"
  [Ra]
  (fn [s]
    (p/map
      (fn [a] [s a])
      Ra)))

(tests
  ;(Ra->SRa (p/resolved 1)) := (fn [s] (p/resolved [s 1]))
  @(runS (Ra->SRa (p/resolved 1)) {})
  := @(runS (fn [s] (p/resolved [s 1])) {})
  := @(p/resolved [{} 1])
  := [{} 1]
  )

(defn hf-pull [pat Ra & [s]]
  (p/map
    (fn [[s' b]] b)
    (runS (hf-pull-SR pat (Ra->SRa Ra)) (or s {}))))

(tests
  @(runS (pureSR 17592186045421) {}) := [{} 17592186045421]

  (second @(runS (hf-eval :dustingetz/gender (pureSR 17592186045421)) {}))
  := (second @(p/resolved ['{dustingetz/gender :dustingetz/male} :dustingetz/male]))

  @(runS (hf-pull-SR :dustingetz/gender (pureSR 17592186045421)) {})
  := [{} #:dustingetz{:gender :dustingetz/male}]

  @(hf-pull :dustingetz/gender (p/resolved 17592186045421))
  := #:dustingetz{:gender :dustingetz/male}

  @(hf-pull :db/ident (p/resolved 17592186045421))
  := #:db{:ident :dustingetz/mens-small}

  @(hf-pull '{:dustingetz/gender :db/ident} (p/resolved 17592186045421))
  := #:dustingetz{:gender #:db{:ident :dustingetz/male}}

  @(hf-pull '(shirt-size dustingetz/gender) (p/resolved nil)
     {'dustingetz/gender (resolved :dustingetz/male)})
  := {'(shirt-size dustingetz/gender) 17592186045421}

  @(hf-pull '{:dustingetz/gender (shirt-size dustingetz/gender)}
     (p/resolved 17592186045421))
  := #:dustingetz{:gender {'(shirt-size dustingetz/gender) 17592186045421}}

  @(hf-pull
     '{:dustingetz/gender
       {(shirt-size dustingetz/gender >needle)
        :db/ident}}
     (p/resolved 17592186045421)
     {'>needle (resolved "large")})
  := @(p/resolved
        {:dustingetz/gender
         {'(shirt-size dustingetz/gender >needle)
          {:db/ident :dustingetz/mens-large}}})

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
