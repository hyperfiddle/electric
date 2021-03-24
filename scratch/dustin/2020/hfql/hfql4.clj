(ns dustin.hfql4
  (:require
    [datomic.api :as d]
    [dustin.fiddle :refer :all]
    [meander.epsilon :as m :refer [match]]
    [minitest :refer [tests]]
    [hyperfiddle.api :as hf]
    [contrib.do :refer [via* Do-via *this !]]
    #_[hyperfiddle.via :refer [via]]))


(comment

  (m/match {1 2}
    {& (m/seqable ?x)}
    ?x)

  (match {:dustingetz/gender [:db/ident]}
    {& (m/seqable [(m/pred keyword? ?k) [!pats]])}
    {?k !pats})
  )

(defn hf-pull [pat v env]                                   ; hf-pull returns a value, not partitions. Not what you want at the top
  #_{:pre  [(doto pat (println 'hf-pull v env))]
   :post [(doto % (println 'hf-pull))]}
  (match pat
    {& (m/seqable [(!xs ...) ?pat])}                        ; one entry
    (let [[f & asks] !xs
          args (map #(get env %) asks)
          v (clojure.core/apply (clojure.core/resolve f) args)
          env (merge env                                    ; shadow uptree env
                {f v})]                                     ; intermediate result is named and available downtree
      {(seq !xs) (hf-pull ?pat v env)})                             ; continue / descend

    {& (m/seqable [(m/pred keyword? ?k) ?pat #_[!pats]])}   ; todo parallel descent if >1 map entries
    (let [v (datomic-nav ?k v)
          env (merge env {?k v})]
      {?k (hf-pull ?pat v env)})

    [!pats ...]
    (->> !pats
      (mapv #(hf-pull % v env))                             ; parallel descent (same env)
      (apply merge))                                        ; collect descendent keyvals into map

    (m/pred keyword? ?x)
    {?x (hf-nav ?x v)}                                      ; no descent, return keyval leaf

    ?_ (doto ?_ (println 'unmatched))
    ))

(tests

  (hf-pull {:dustingetz/gender [:db/ident]} 17592186045441 {})
  := #:dustingetz{:gender #:db{:ident :dustingetz/male}}

  (hf-pull {:dustingetz/gender [:db/ident :db/id]} 17592186045441 {})
  := #:dustingetz{:gender #:db{:ident :dustingetz/male, :id 17592186045430}}

  ;(hf-pull {:dustingetz/gender [:db/ident :db/id '(identity ident)]} 17592186045441 {})

  (d/pull hf/*$* [:dustingetz/gender] 17592186045441)
  := #:dustingetz{:gender #:db{:id 17592186045430}}

  (hf-pull :dustingetz/gender 17592186045441 {})
  := #:dustingetz{:gender #:db{:id 17592186045430}}

  (hf-pull [:dustingetz/gender] 17592186045441 {})
  := #:dustingetz{:gender :dustingetz/male}                 ; !
  ;:= #:dustingetz{:gender #:db{:id 17592186045430}}

  (hf-pull '{(submission needle) [:dustingetz/gender :dustingetz/email]} nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender :dustingetz/female, :email "alice@example.com"}}
  ;:= '{(submission needle) #:dustingetz{:gender #:db{:id 17592186045431}, :email "alice@example.com"}}

  (hf-pull '{(submission needle) [{:dustingetz/gender [:db/id :db/ident]}
                                  {(identity needle) []}
                                  :dustingetz/email]}
    nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender #:db{:id 17592186045431, :ident :dustingetz/female},
                                        :email "alice@example.com"}}

  (hf-pull '{(submission needle) [:dustingetz/gender :dustingetz/email]} nil {'needle "ch"})
  := '{(submission needle) #:dustingetz{:gender #:db{:id 17592186045430}, :email "charlie@example.com"}}

  (hf-pull '{(submission needle) [{:dustingetz/gender [:db/ident]} :dustingetz/email]} nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender #:db{:ident nil}, :email "alice@example.com"}}
  ;:= '{(submission needle) #:dustingetz{:gender [:db/ident], :email "alice@example.com"}}


  ;(hf-pull '[{(submission needle) [{:dustingetz/gender
  ;                                  [:db/ident
  ;                                   {(shirt-size dustingetz/gender needle2) [*]}]}]}
  ;           (genders)]
  ;  nil {'needle "alic"
  ;       'needle2 "large"})
  ;:= nil
  ; Produce a reactive result at the end, the env is reactive too but internal state
  ; Question is how to bridge this over network so (shirt-size ... "foo") reloads efficiently
  ; That means the intermediate streams have to be available


  ; 1. make this work
  ; 2. make it work with reactive environment
  ; 3. how to optimize it
  ; 4. cardinality

  ; a symbolic trace with keys like (submisson needle) can give us the final result, but not how it evaluated
  ;

  ; Note the UI does not need the final result, it needs reactive partitions for key reactive inputs
  )

(defrecord HFQL [scope trace]
  Do-via
  (resolver-for [_]
    {:Hfql.get-trace (fn [[_]] (:trace *this))
     :Hfql.pull
     (fn [[_ pat v]]
       (match pat
         {& (m/seqable [(!xs ...) ?pat])}                   ; one entry
         (let [[f & asks] !xs
               args (map #(get scope %) asks)
               v (apply (clojure.core/resolve f) args)
               scope (merge scope {f v})]                              ; intermediate result is named in scope, shadow uptree
           (set! *this (update *this :trace assoc scope `(f ~v) v))
           {(seq !xs) (hf-pull ?pat v scope)})              ; continue / descend

         {& (m/seqable [(m/pred keyword? ?k) ?pat #_[!pats]])} ; todo parallel descent if >1 map entries
         (let [v (datomic-nav ?k v)
               scope (merge scope {?k v})]
           (set! *this (update *this :trace assoc scope `(f ~v) v))
           {?k (hf-pull ?pat v scope)})

         [!pats ...]
         (->> !pats
           (mapv #(hf-pull % v scope))                      ; parallel descent (same scope)
           (apply merge))                                   ; collect descendent keyvals into map

         (m/pred keyword? ?x)
         {?x (hf-nav ?x v)}                                      ; no descent, return keyval leaf

         ?_ (doto ?_ (println 'unmatched))
         ))}))

(tests
  (via* (->HFQL {})
    (! :Hfql.pull {:dustingetz/gender [:db/ident]} 17592186045441)
    (! :Hfql.get-trace)
    )
  )