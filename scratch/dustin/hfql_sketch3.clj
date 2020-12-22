(ns dustin.hfql_sketch3)


(ns scratch-69)


(declare Stream)

(defn hfql [ast]
  (fn [env]                                                 ; env :: Map[Sym->V]
    ; env :: Stream(Map[Sym->(Stream V)])

    ))


(def ast '[{(submissions $ needle) [{:gender [:db/ident
                                              {(shirt-sizes $ gender needle12) [*]}]}]}
           {(genders $ needle2) [*]}])

(defn ^:hyperfiddle.api/fiddle foo [>$ >needle]
  (via (Stream.)
    (hfql ast ~{'$ ~>$ 'needle ~>needle})))

(s/fdef foo
  :args (s/cat :$ stream? :>needle stream?)
  :ret stream?)


;                database \
; interaction -> server -> hfql -> view -> interaction

(defn edn-view [>partitions]                                 ; ::
  (via (Stream)
    (vector :pre (js/pprint-str ~>partitions)))
  )


(def >needle (input))
(def >needle2 (input))
(def >needle10 (input))
(def >needle11 (input))
(def >needle12 (input))
(def >$ (input))
(def >vdom (edn-view (foo >$ >needle)))

[
 [needle -> ...]
 [needle2 -> ...]
 [needle10 -> vdom10]
 [needle11 -> vdom11]
 [needle12 -> vdom12]
 [[vdom10 vdom11 vdom12] -> vdom]
 ]

(via (stream) (shirt-sizes ~(:gender ~(submissions ~needle))))
(via (stream) ((comp :gender submissions shirt-sizes) ~needle))




(on >vdom reconcile!)

(put >$ ...)
(put >needle "foo")


(def c
  '#{(submissions $ '[{:gender [:db/ident]}] needle)
     (:gender (submissions $ '[{:gender [:db/ident]}] needle))
     (shirt-sizes $ '[*] (:gender (submissions $ '[{:gender [:db/ident]}] needle)))
     (genders $ '[*])})

refactor:=

'#{(shirt-sizes $ '[*] (:gender (submissions $ '[{:gender [:db/ident]}] needle)))
   (genders $ '[*])}


(def >needle (input))
(def >$ (input))


; Note I have removed the cardinality, and marked the stream inputs vs the traced inputs
'[{(submission >$ >needle) [{:gender [:db/ident
                                      {(shirt-size >$ gender) [*]}]}]}]

:=
; One stream-fmap is enough therefore there is only one partition here
(stream/fmap (fn [$ needle]
               '[{(submission $ needle) [{:gender [:db/ident {(shirt-size $ gender) [*]}]}]}])
  >$ >needle)
; But we still had to trace the evaluation to resolve 'gender

:=
; The single partition, a# form elided for clarity
'{a# #:scratch{:email  "alice@example.com",
               :gender {:db/ident  :female,
                        shirt-size #:db{:ident :scratch/womens-medium}}}}

; Remove the >$
; Note I have removed the cardinality, and marked the stream inputs vs the traced inputs
'[{(submission >needle) [{:gender [:db/ident {(shirt-size gender) [*]}]}]}]

:=
; One stream-fmap is enough therefore there is only one partition here
(stream/fmap (fn [needle]
               '[{(submissio needle) [{:gender [:db/ident {(shirt-size gender) [*]}]}]}])
  >needle)
; But we still had to trace the evaluation to resolve 'gender

:=
; The single partition, a# form elided for clarity
'{a# #:scratch{:email  "alice@example.com",
               :gender {:db/ident  :female,
                        shirt-size #:db{:ident :scratch/womens-medium}}}}


((hfql '[{(submission >needle) [{:gender [:db/ident {(shirt-size gender) [*]}]}]}]) env)
:= (via (stream)
     (hf-pull [{(submission ~>needle) [{:gender [:db/ident {(shirt-size gender) [*]}]}]}]))
:= (stream/fmap
     (fn [needle]
       (hf-pull [{(submission needle) [{:gender [:db/ident {(shirt-size gender) [*]}]}]}]))
     >needle)
