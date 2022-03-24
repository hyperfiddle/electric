(ns geoffrey.hfql-examples3)
​

(def a '[{(submissions needle) [{:gender [:db/ident
                                          {(shirt-sizes gender) [*]}]}]}
         {(genders) [*]}])
​
(def b '#{(submissions '[{:gender [:db/ident]}] needle)
          (:gender (submissions '[{:gender [:db/ident]}] needle))
          (shirt-sizes '[*] (:gender (submissions '[{:gender [:db/ident]}] needle)))
          (genders '[*])})
​
(def c '#{(submissions $ '[{:gender [:db/ident]}] needle)
          (:gender (submissions $ '[{:gender [:db/ident]}] needle))
          (shirt-sizes $ '[*] (:gender (submissions $ '[{:gender [:db/ident]}] needle)))
          (genders $ '[*])})
​
(def d '{(submissions $ '[{:gender [:db/ident]}] needle)                                (submissions (ask '$) '[{:gender [:db/ident]}] (ask 'needle))
         (:gender (submissions $ '[{:gender [:db/ident]}] needle))                      (:gender (ask '(submissions $ '[{:gender [:db/ident]}] needle)))
         (shirt-sizes $ '[*] (:gender (submissions $ '[{:gender [:db/ident]}] needle))) (shirt-sizes (ask '$) '[*] (ask '(:gender (submissions $ '[{:gender [:db/ident]}] needle))))
         (genders $ '[*])                                                               (genders (ask '$) '[*])})
​
(def e '{(shirt-sizes $ '[*] (:gender (submissions $ '[{:gender [:db/ident]}] needle))) (fmap (fn [sym0 sym1] (shirt-sizes sym0 '[*] sym1)) (ask '$) (ask '(:gender (submissions $ '[{:gender [:db/ident]}] needle))))
         (genders $ '[*])                                                               (fmap (fn [sym2] (genders sym2 '[*])) (ask '$))
         (submissions $ '[{:gender [:db/ident]}] needle)                                (fmap (fn [sym3 sym4] (submissions sym3 '[{:gender [:db/ident]}] sym4)) (ask '$) (ask 'needle))
         (:gender (submissions $ '[{:gender [:db/ident]}] needle))                      (fmap :gender (ask '(submissions $ '[{:gender [:db/ident]}] needle)))})
​
(comment
  ;; After eval in Dataflow context
  (def g View{'$                                       Input
              '(submissions $ [{:gender [:db/ident]}]) View
              '(:gender View)                          View
              '(shirt-sizes $ [*] View)                View
              '(genders $ [*])                         View})
  ​
  ;; After eval in Maybe context
  (def g (Just {'$                                         (Just $)
                '(submissions $ [{:gender [:db/ident]}])   (Just computed-value)
                '(:gender (Just computed-value))           (Just computed-value)
                '(shirt-sizes $ [*] (Just computed-value)) (Just computed-value)
                '(genders $ [*])                           (Just computed-value)}))
  ​
  ;; After snapshot, in the EDN view
  (def g {'$                                       $
          '(submissions $ [{:gender [:db/ident]}]) computed-value
          '(:gender computed-value)                computed-value
          '(shirt-sizes $ [*] computed-value)      computed-value
          '(genders $ [*])                         computed-value}))
​
