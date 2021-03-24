(ns dustin.hfql16
  (:refer-clojure :exclude [sequence])
  (:require [dustin.fiddle :refer :all]
            [dustin.hf-nav :refer :all]
            [meander.epsilon :as m :refer [match]]
            [minitest :refer [tests]]
            [promesa.core :as p]))


(defn pureSR [a]
  (fn [s] (p/resolved [s a])))

(defn runS [SMa s]
  (SMa s))

(defn bindSR [SRa f]
  (fn [s]
    (p/then (runS SRa s)
      (fn [[s' a]]
        (runS (f a) (merge s s'))))))

(tests
  ;(runStateT (pureSR 1) {}) := (p/resolved [{} 1]) ; fails
  @(runS (pureSR 1) {}) := @(p/resolved [{} 1])

  @(runS
     (bindSR (pureSR 1)
       (fn [a]
         (fn [s]
           (p/resolved
             [s (+ a (get s 'C))]))))
     {'C 42})
  := @(p/resolved [{'C 42} 43])
  )

(defn fmapSR [f SRa]
  (fn [s]
    (p/map
      (fn [[s' a]]
        [s (f a)])
      (runS SRa s))))

;(defn fmap [f & Sas]
;  (fn [s]
;    (let [as (->> Sas
;               (map (fn [Sa]
;                      (let [[s' a] (run Sa s)]
;                        a))))]
;      [s (apply f as)])))

(tests
  @(runS (fmapSR inc (pureSR 1)) {})
  := @(p/resolved [{} 2])

  ;(runS (pureSR 1) {}) := [{} 1]
  ;(runS (fmapSR + (pureSR 1) (pureSR 2)) {}) := [{} 3]

  )

;(defn sequenceSR [mas]
;  (apply fmapSR vector mas))

;(tests
;  (run (sequence []) {}) := [{} []]
;  (run (sequence [(pure 1) (pure 2)]) {}) := [{} [1 2]]
;  )

; ---

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(defn hf-apply [edge a scope]
  (cond
    (keyword? edge) (hf-nav edge a)
    (seq? edge) (let [[f & args] edge]
                  (apply (clojure.core/resolve f) (replace scope args)))
    () (println "hf-eval unmatched edge: " edge)))

(defn hf-eval [edge SRa]
  (fn [s]
    (p/map
      (fn [[s' a]]
        (let [b (hf-apply edge a (merge s s'))]
          [(merge s {(hf-edge->sym edge) b})
           b]))
      (runS SRa s))))

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

  @(runS (hf-eval :dustingetz/gender (pureSR 17592186045421)) {})
  := @(p/resolved ['{dustingetz/gender :dustingetz/male} :dustingetz/male])

  @(runS (hf-pull-SR :dustingetz/gender (pureSR 17592186045421)) {})
  := [{} #:dustingetz{:gender :dustingetz/male}]

  @(hf-pull :dustingetz/gender (p/resolved 17592186045421))
  := #:dustingetz{:gender :dustingetz/male}

  @(hf-pull :db/ident (p/resolved 17592186045421))
  := #:db{:ident :dustingetz/mens-small}

  @(hf-pull '{:dustingetz/gender :db/ident} (p/resolved 17592186045421))
  := #:dustingetz{:gender #:db{:ident :dustingetz/male}}

  @(hf-pull '(shirt-size dustingetz/gender) (p/resolved nil)
     {'dustingetz/gender :dustingetz/male})
  := {'(shirt-size dustingetz/gender) 17592186045421}

  @(hf-pull '{:dustingetz/gender (shirt-size dustingetz/gender)}
     (p/resolved 17592186045421))
  := #:dustingetz{:gender {'(shirt-size dustingetz/gender) 17592186045421}}

  @(hf-pull
     '{:dustingetz/gender
       {(shirt-size dustingetz/gender)
        :db/ident}}
     (p/resolved 17592186045421))
  := @(p/resolved
        {:dustingetz/gender
         {'(shirt-size dustingetz/gender)
          {:db/ident :dustingetz/mens-small}}})

  )
