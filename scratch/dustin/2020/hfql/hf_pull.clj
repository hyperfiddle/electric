(ns dustin.hf-pull
  (:require
    [meander.epsilon :as m :refer [match]]
    [minitest :refer [tests]]))


(defn submission [& args] ::x)
(defn shirt-size [& args] ::y)

; ---

(comment
  (defn hfql [ast]                                          ; return reactive partitions
    (fn [env]
      ...))

  (tests
    (hfql '{})
    := {'a >a
        'b >b}
    )
  )

(comment
  '{(submission :needle) [{:dustingetz/gender
                           [:db/ident
                            {(shirt-size ~:dustingetz/gender ~>needle2) [*]}]}]}

  (def >needle (pure "alice"))
  (def >needle2 (pure "large"))
  (def e (d/entity hf/*$*))

  ; (a) individual root partition
  (hf-pull [:scratch/email
            {:scratch/gender [:db/ident {(shirt-size gender) [*]}]}] ; sexpr = just a keyword (but on steroids)
    (submission "alice"))
  := #:scratch{:email  "alice@example.com",
               :gender {:db/ident  :female,
                        shirt-size #:db{:ident :scratch/womens-medium}}}

  ; (b) Reactive root partition
  (via (stream)
    ((hfql ~[~{~(submission ~>needle) [:scratch/email {:scratch/gender [:db/ident {(shirt-size gender) [*]}]}]}])
     {}))                                                   ; empty env. is >needle in env?
  ; What are the partition keys?
  := {(hf-pull (submission needle) [:scratch/email {:scratch/gender [:db/ident {(shirt-size gender) [*]}]}])
      #:scratch{:email  "alice@example.com",
                :gender {:db/ident  :female,
                         shirt-size #:db{:ident :scratch/womens-medium}}}}

  ; (c) Reactive cardinality 1 sub partition - add >needle2
  (via (stream)
    (hfql ~[~{~(submission ~>needle) ~[:scratch/email
                                       ~{:scratch/gender ~[:db/ident
                                                           ~{~(shirt-size gender ~>needle2) [*]}]}]}]))

  (hfql ~[~{~(submission ~>needle) ~[:scratch/email
                                     ~{:scratch/gender ~[:db/ident
                                                         ~(hfql ~{~(shirt-size gender ~>needle2) [*]})]}]}])

  (let [cont (fn [>needle2 env]
               ; HFQL accepts an env, returns a stream. The env unwinds thus not needed to return. Re-enter if you want that.
               (via (stream)
                 (~(hfql ~{~(shirt-size gender needle2) [*]})
                  (assoc env 'needle2 ~>needle2))))]
    (hfql ~[~{~(submission ~>needle) ~[:scratch/email
                                       ~{:scratch/gender ~[:db/ident ; keywords get an env-ish implicitly. and a continuation?
                                                           ~(cont >needle2)]}]}]))


  ; Write out the environment continuations
  (def env {})
  (hfql [{(submission >needle env)
          [(:scratch/email env)
           {(:scratch/gender env)
            [(:db/ident env)
             {(shirt-size >needle2 env)
              [(* env)]}]}]}])

  (hf-pull '{(submission needle) [:scratch/gender :scratch/email]}
    nil
    {:needle "alice"})
  := '{submission #:scratch{:gender nil, :email nil}}


  '{submission #:scratch{:gender {shirt-size #:db{:ident :scratch/womens-medium}
                                  :db/ident  :female}
                         :email  "alice@example.com"}}


  ; (d) in (c) the EDN view explodes into a fabric of micro-partitions, i think this is canonical expansion
  ; But, (c) can be rewritten into larger partition batches without loss of performance (this is the hard part)
  (via (stream)
    (let [[enva >a] (hfql ~[~{~(submission ~>needle) ~[:scratch/email
                                                      ~{:scratch/gender ~[:db/ident
                                                                          MARK]}]}])
          ; but the env unwinds

          ; if the MARK is not reactive, eval inline
          ; If the mark IS reactive, allocate a partition

          [envb >b] ((hfql '{~(shirt-size gender ~>needle2) [*]}) env)]

      ; partitions. They could be sequenced on the client into a final EDN value
      #_(merge ~enva ~envb)
      {'a >a 'b >b}
      )
    )

  )

; Cardinality N
(comment
  (defn submissions [needle] ...)
  (defn shirt-sizes [gender] ...)

  (tests

    ; Cardinality N
    (hf-pull [:scratch/email
              {:scratch/gender [:db/ident {(shirt-sizes gender) [*]}]}]
      (submissions "alice"))

    ; Note that Datomic keywords already handle cardinality in pull expressions
    ; Use a state monad to trace the evaluation across cardinality

    ))



; Reactive cardinality N sub partition - add cardinality to >needle2