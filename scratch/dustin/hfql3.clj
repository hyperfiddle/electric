(ns dustin.hfql3
  (:require
    [datomic.api :as d]
    [hyperfiddle.api :as hf]
    [meander.epsilon :refer [match]]
    [minitest :refer [tests]]))


(tests
  (def e [:dustingetz/email "alice@example.com"])
  (d/pull hf/*$* [:dustingetz/gender] e)

  (select-keys {:dustingetz/email "a"} [:dustingetz/email]) := #:dustingetz{:email "a"}
  (get {:dustingetz/email "a"} :dustingetz/email) := "a"
  (:dustingetz/email {:dustingetz/email "a"}) := "a"

  (d/q '[:find (pull ?e [*])
         :where [?e :dustingetz/email]] hf/*$*)

  )

(defn gender [] {:db/ident ::male})

(defn hf-pull [pp e]                                      ; No AST stuff
  ; HF pulls through not just keywords, but also functions
  ; Keywords are pointfree functions
  ; Sexpr forms need not be pointfree, the pull-bindings are in their lexical env
  ;(println 'e e 'pp pp)
  (cond
    (vector? pp) (into {}
                   (map (fn [k]                             ; #_(d/pull hf/*$* pp e)
                          [k (hf-pull k e)]))
                   pp)
    (seq? pp) (eval pp)                                     ; with pull binds in scope and lexical scope
    (keyword? pp) (get (d/pull hf/*$* [pp] e) pp)           ; lol
    (fn? pp) (pp e)
    ))

(tests
  ; No reactions here

  (hf-pull ':dustingetz/gender e) := #:db{:id 17592186045431}
  (hf-pull '[:dustingetz/gender] e) := #:dustingetz{:gender #:db{:id 17592186045431}}
  (hf-pull '{:dustingetz/gender [:db/ident]} e) :=
  (hf-pull '[{:dustingetz/gender [:db/ident]}] e) := #:dustingetz{:gender #:db{:ident :dustingetz/female}}

  (hf-pull '(gender) e) := #:db{:ident :dustin.hfql3/male}
  (hf-pull '[(gender)] e) := '{(gender) #:db{:ident :dustin.hfql3/male}}
  (hf-pull '{(gender) [:db/ident]} e) :=
  (hf-pull '[{(gender) [:db/ident]}] e) :=

  (hf-pull '{(gender) [*]} e) :=
  (hf-pull '{(submission needle) [:dustingetz/gender]} e) :=
  (hf-pull '{(submission needle) [:dustingetz/gender]} e) :=

  (hf-pull '[{(gender) [*]}] e) :=

  (hf-pull '[{(gender) [*]}
             {(submission needle) [:dustingetz/gender]}] e) :=

  (hf-pull '[{(gender)            [*]
              (submission needle) [:dustingetz/gender]}] e) :=

  (hf-pull '{:dustingetz/gender [:db/ident]} e) :=
  (hf-pull '[{:dustingetz/gender [:db/ident]}] e) :=
  (hf-pull '{(shirt-size gender needle2) [*]} e) :=

  (hf-pull '[{:dustingetz/gender
              [:db/ident
               {(shirt-size gender needle2) [*]}]}] e) :=
  (hf-pull '{(submission needle) [{:dustingetz/gender
                                   [:db/ident
                                    {(shirt-size gender needle2) [*]}]}]} e) :=

  )

; hfql :: [stream[user-input]] -> stream[map[sexp->stream[result]]]
; hfql :: [user-input] -> map[sexp->partition] NOT THIS ONE
(defn hfql [pp env]
  ; writer-reader aware fn that returns
  ; is it stream aware?  YES, to react shirt-sizes but not submission
  {'(submission "needle1")                    (pure {:dustingetz/email "alice@" :dustingetz/name "alice"})
   '(shirt-size :dustingetz/female "needle2") ...
   ;'(shirt-size :dustingetz/male "needle2") ...
   })

(tests
  (hfql '{(submission needle) [{:dustingetz/gender
                                [:db/ident
                                 {(shirt-size gender needle2) [*]}]}]} e)
  := {'(submission needle) ...
      '(shirt-size gender needle2) ...}
  )

(tests

  (hfql "needle1" "needle2")
  (via ...
    (hfql >needle1 >needle2)
    ;(hfql ~>needle1 ~>needle2)
    )

  := (fapply (pure hfql) >needle1 >needle2)



  (hfql ':dustingetz/gender e) := #:db{:id 17592186045431}
  (hfql '[:dustingetz/gender] e) := #:dustingetz{:gender #:db{:id 17592186045431}}
  (hfql '{:dustingetz/gender [:db/ident]} e) :=
  (hfql '[{:dustingetz/gender [:db/ident]}] e) := #:dustingetz{:gender #:db{:ident :dustingetz/female}}

  (hfql '(gender) e) := #:db{:ident :dustin.hfql3/male}
  (hfql '[(gender)] e) := '{(gender) #:db{:ident :dustin.hfql3/male}}
  (hfql '{(gender) [:db/ident]} e) :=
  (hfql '[{(gender) [:db/ident]}] e) :=
  (hfql '{(submission :needle) [{:dustingetz/gender
                                 [:db/ident
                                  {(shirt-size ~:dustingetz/gender ~>needle2) [*]}]}]} e)

  ; env
  {:dustingetz/gender (pure :dustingetz/male)
   :needle  (pure "needle")
   :needle2 (pure "needle2")}

  )