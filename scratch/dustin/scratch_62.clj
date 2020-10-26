(ns dustin.scratch-62)


; These are simple datomic functions. No reactive stuff, no gymnastics
; End users can only write very simple functions. Datomic knowledge only no HF semantics
(defn submission [needle]
  (d/q '[:find ?e . :in $ ?needle
         :where
         [?e :dustingetz/email]
         [?e :dustingetz/name ?needle]]
    hf/*$* needle))

(defn gender [] (d/q '[:find ?e . :where [_ :dustingetz/gender ?e]] hf/*$*))

(defn shirt-size [gender needle]
  (d/q '[:find ?e . :in $ ?gender :where [?e :dustingetz/gender ?gender]]))

; Gymnastics be at this layer
(defn ^::hf/fiddle submission-masterlist [>needle >needle2]
  (via ...                                                  ; AST stuff here
    (hf-pull
      [{'(gender) [*]}
       {'(submission ~>needle) [{:dustingetz/gender
                                 [:db/ident
                                  {'(shirt-size gender ~>needle2) [*]}]}]}])))

(defn ^::hf/fiddle submission-masterlist [needle needle2]
  (hfql
    [{'(gender) [*]}
     {'(submission needle) [{:dustingetz/gender
                             [:db/ident
                              {'(shirt-size gender needle2) [*]}]}]}]))


(fapply (fn [needle needle2]
          (hf-pull
            [{'(gender) [*]}
             {'(submission >needle) [{:dustingetz/gender
                                      [:db/ident
                                       {'(shirt-size gender >needle2) [*]}]}]}])
          )
  >needle >needle2)



; Thread macros not reactive
[(->> (gender) (hf/pull hf/*$* ['*]))
 (->> (submission ~>needle) (hf/pull hf/*$* [{:dustingetz/gender
                                              [:db/ident
                                               {(shirt-size gender ~>needle2) [*]}]}]))
 ]
