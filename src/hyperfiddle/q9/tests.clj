(ns hyperfiddle.q9.tests
  (:require [clojure.spec.alpha :as s]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.q9 :refer [hfql]]
            [hyperfiddle.q9.impl :refer [dependencies parse reverse-deps references toposort]]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [user.gender-shirt-size :refer [submissions submission shirt-sizes genders]]))

;; (rcf/enable!)
;; (rcf/enable! false)

(s/fdef submissions :args (s/cat :needle string?) :ret (s/coll-of any?))
(s/fdef genders :ret (s/coll-of any?))

(s/fdef shirt-sizes :args (s/cat :gender keyword?, :needle string?) :ret (s/coll-of any?))
(def test-form '[{(hyperfiddle.q9/submissions "") [:user/email
                                                   {(:user/gender . ::hf/options hyperfiddle.q9/genders) [(:db/ident . ::hf/as gender)]}
                                                   {(:user/shirt-size . ::hf/options #'(shirt-sizes gender)) [:db/ident]}]}
                 {(hyperfiddle.q9/genders) [:db/ident]}])

(tests
 (let [deps (dependencies (parse test-form))]
   (reverse-deps (reverse-deps deps)) := deps))

(tests
 (references '{genders #{}} (parse '(:user/gender . ::hf/options genders))) := #{'genders}
 (references (dependencies (parse '(:db/ident . ::hf/as gender)))
             (parse '{(:user/shirt-size . ::hf/options (shirt-sizes gender user/gender)) [:db/id]})) := #{'gender}
 )

(tests (dependencies (parse test-form))
       := '{gender                                                      #{},
            nav__db_ident_1311322104                                    #{},
            traverse__hyperfiddle.q9_submissions_-882945936->-413203446 #{collect__-413203446},
            collect__-413203446                                         #{nav__user_email_-1193137536
                                                                          traverse__user_gender_-1061963905->768966341
                                                                          traverse__user_shirt-size_-1547882470->2075314760},
            nav__user_email_-1193137536                                 #{},
            traverse__hyperfiddle.q9_genders_1712563381->2075314760     #{collect__2075314760},
            collect__2075314760                                         #{nav__db_ident_1311322104},
            collect__768966341                                          #{gender},
            traverse__user_gender_-1061963905->768966341                #{collect__768966341},
            traverse__user_shirt-size_-1547882470->2075314760           #{collect__2075314760},
            collect__340489282                                          #{traverse__hyperfiddle.q9_submissions_-882945936->-413203446
                                                                          traverse__hyperfiddle.q9_genders_1712563381->2075314760}})

(tests
 (toposort (dependencies (parse test-form))) := '{gender                                                      0,
                                                  nav__db_ident_1311322104                                    0,
                                                  traverse__hyperfiddle.q9_submissions_-882945936->-413203446 4,
                                                  collect__-413203446                                         3,
                                                  nav__user_email_-1193137536                                 0,
                                                  traverse__hyperfiddle.q9_genders_1712563381->2075314760     2,
                                                  collect__2075314760                                         1,
                                                  collect__768966341                                          1,
                                                  traverse__user_gender_-1061963905->768966341                2,
                                                  traverse__user_shirt-size_-1547882470->2075314760           2,
                                                  collect__340489282                                          5})


(tests
 (p/run (! (binding [hf/entity 9] (hfql :db/id) )))
 % := 9)

(tests
 (p/run (! (binding [hf/entity 9] (hfql [:db/id]) )))
 % := {:db/id 9})

(p/def string-renderer #'(str ~hf/value))

(tests
 "hf/render"
 (p/run (! (binding [hf/entity 9] (hfql (:db/id . ::hf/render string-renderer)) )))
 % := "9")

(tests
 "hf/render inline"
 (p/run (! (binding [hf/entity 9] (hfql (:db/id . ::hf/render #'(str ~hf/value))) )))
 % := "9")

(tests
 (p/run (! (binding [hf/entity 9] (hfql [(:db/id . ::hf/render string-renderer)]) )))
 % := {:db/id "9"})

(tests
 (p/run (binding [hf/entity 9] (! (hfql {:dustingetz/gender [:db/ident]}) )))
 % := {:dustingetz/gender {:db/ident :dustingetz/female}})

(tests
 (p/run (binding [hf/entity 9] (! (hfql [{:dustingetz/gender [:db/ident]}]) )))
 % := {:dustingetz/gender {:db/ident :dustingetz/female}})

(tests
 (p/run (! (hfql {(submission "") [:db/id]}) ))
 % := {'(user.gender-shirt-size/submission "") {:db/id 9}})

(tests
 "EAV"
 (p/run (! (hfql {(submission "") [(:dustingetz/email . ::hf/render #'[hf/entity hf/attribute ~hf/value])]}) ))
 % := '{(user.gender-shirt-size/submission "") {:dustingetz/email [9 :dustingetz/email "alice@example.com"]}})

(tests
 "Two levels of nesting"
 (p/run (! (hfql {(submission "") [{:dustingetz/shirt-size [:db/ident]}]}) ))
 % := {'(user.gender-shirt-size/submission "") #:dustingetz{:shirt-size #:db{:ident :dustingetz/womens-large}}})

(tests
 "multiplicity many"
 (p/run (! (hfql {(submissions "") [:db/id]}) ))
 % := {'(user.gender-shirt-size/submissions "") [{:db/id 9} {:db/id 10} {:db/id 11}]})

(tests
 (p/run (! (hfql {(submissions "") [(:db/id . ::hf/render string-renderer)]}) ))
 % := {'(user.gender-shirt-size/submissions "") [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})



(defn fail []
  (throw (ex-info "I fail" {})))

(p/def throwing-renderer #'(fail))

(p/def ignoring-renderer #'"ignored")

(s/def :dustingetz/gender any?)

(tests
 (p/run (! (binding [hf/entity 9]
             (hfql [{(:dustingetz/gender . ::hf/render ignoring-renderer) [(:db/ident . ::hf/render throwing-renderer)]}]) )))
 % := #:dustingetz{:gender "ignored"}
 ;; note it didn’t throw
 )



(p/def select-option-renderer
  #'(into [:select {:value ~hf/sequenceM}]
          (p/for [e ~(::hf/options hf/props)]
            [:option e])))

(tests
 (p/run (! (binding [hf/entity 9]
             (hfql (:dustingetz/shirt-size . ::hf/render select-option-renderer
                                             ::hf/options #'(p/$ shirt-sizes :dustingetz/female ""))) 
             )))
 %
 := [:select {:value 8} [:option 6] [:option 7] [:option 8]]
 )


(tests
 (p/run (! (binding [hf/entity 9]
             (hfql {(:dustingetz/shirt-size . ::hf/render select-option-renderer
                                              ::hf/options #'(p/$ shirt-sizes :dustingetz/female ""))
                    [:db/ident]}) )))
 % := {:dustingetz/shirt-size [:select {:value #:db{:ident :dustingetz/womens-large}}
                               [:option #:db{:ident :dustingetz/womens-small}]
                               [:option #:db{:ident :dustingetz/womens-medium}]
                               [:option #:db{:ident :dustingetz/womens-large}]]})

;;;;;;;;;;;;;;;
;; ENV TESTS ;;
;;;;;;;;;;;;;;;

(tests
 (p/run (binding [hf/entity 9]
          (! (hfql [(:dustingetz/shirt-size . ::hf/options #'(p/$ shirt-sizes gender ""))
                    {:dustingetz/gender [(:db/ident . ::hf/as gender)]}]) 
             )))
 % := #:dustingetz{:gender     #:db{:ident :dustingetz/female},
                   :shirt-size 8})

(p/defn shirt-sizes-renderer []
  ~(::hf/options hf/props))

(tests
 (p/run (binding [hf/entity 9]
          (! (hfql [{:dustingetz/gender [(:db/ident . ::hf/as gender)]}
                    {(:dustingetz/shirt-size . ::hf/options #'(p/$ shirt-sizes gender "")
                                             ::hf/render shirt-sizes-renderer)
                     [:db/ident]}]) )))
 % := {:dustingetz/gender     {:db/ident :dustingetz/female},
       :dustingetz/shirt-size [{:db/ident :dustingetz/womens-small}
                               {:db/ident :dustingetz/womens-medium}
                               {:db/ident :dustingetz/womens-large}]})

(tests
 "env under card-n"
 (p/run (binding [hf/entity 9]
          (! (hfql {(submissions "") [{:dustingetz/gender [(:db/ident . ::hf/as gender)]}
                                      {(:dustingetz/shirt-size . ::hf/options #'(p/$ shirt-sizes gender "")
                                                               ::hf/render shirt-sizes-renderer)
                                       [:db/ident]}]}) )))
 % := '{(user.gender-shirt-size/submissions "")
        [{:dustingetz/gender {:db/ident :dustingetz/female},
          :dustingetz/shirt-size
          [{:db/ident :dustingetz/womens-small}
           {:db/ident :dustingetz/womens-medium}
           {:db/ident :dustingetz/womens-large}]}
         {:dustingetz/gender {:db/ident :dustingetz/male},
          :dustingetz/shirt-size
          [{:db/ident :dustingetz/mens-small}
           {:db/ident :dustingetz/mens-medium}
           {:db/ident :dustingetz/mens-large}]}
         {:dustingetz/gender {:db/ident :dustingetz/male},
          :dustingetz/shirt-size
          [{:db/ident :dustingetz/mens-small}
           {:db/ident :dustingetz/mens-medium}
           {:db/ident :dustingetz/mens-large}]}]})
;;;;;;;;;;;;
;; NEEDLE ;;
;;;;;;;;;;;;

;; DONE
(tests
 "needle resolves from lexical env"
 (let [needle "alice"]
   (p/run (! (hfql {(submissions needle) [:db/id]}) 
             )))
 % := '{(user.gender-shirt-size/submissions needle) [#:db{:id 9}]})

(p/defn render-typeahead []
  [:select {:value ~hf/sequenceM}
   (p/for [e (binding [hf/args {'needle ""}] ;; TODO
                 ~(::hf/options hf/props))]
     [:option e])])

(tests
 "Free input"
 (p/run (! (binding [hf/args {'needle "alice"}]
             (hfql {(submissions needle) [:db/id]}))))
 % := '{(user.gender-shirt-size/submissions needle) [#:db{:id 9}]})

;; DONE
(tests
 "Two `needle` deep inputs. Not defined in lexical scope."
  (p/run (! (hfql {(submissions needle)
                   [:db/id
                    :dustingetz/email
                    {(:dustingetz/shirt-size . ::hf/render render-typeahead
                                             ::hf/options (shirt-sizes gender needle))
                     [:db/ident]}
                    {:dustingetz/gender [(:db/ident . ::hf/as gender)]}]}) ))
  % := '{(user.gender-shirt-size/submissions needle)
         [{:dustingetz/gender {:db/ident :dustingetz/female},
           :dustingetz/email "alice@example.com",
           :dustingetz/shirt-size
           [:select
            {:value {:db/ident :dustingetz/womens-large}}
            [[:option {:db/ident :dustingetz/womens-small}]
             [:option {:db/ident :dustingetz/womens-medium}]
             [:option {:db/ident :dustingetz/womens-large}]]],
           :db/id 9}
          {:dustingetz/gender {:db/ident :dustingetz/male},
           :dustingetz/email "bob@example.com",
           :dustingetz/shirt-size
           [:select
            {:value {:db/ident :dustingetz/mens-large}}
            [[:option {:db/ident :dustingetz/mens-small}]
             [:option {:db/ident :dustingetz/mens-medium}]
             [:option {:db/ident :dustingetz/mens-large}]]],
           :db/id 10}
          {:dustingetz/gender {:db/ident :dustingetz/male},
           :dustingetz/email "charlie@example.com",
           :dustingetz/shirt-size
           [:select
            {:value {:db/ident :dustingetz/mens-medium}}
            [[:option {:db/ident :dustingetz/mens-small}]
             [:option {:db/ident :dustingetz/mens-medium}]
             [:option {:db/ident :dustingetz/mens-large}]]],
           :db/id 11}]})

;; FAIL genders ref on ::hf/options is not handled properly
(tests
 "Two `needle` deep inputs. Not defined in lexical scope."
 (p/run (! (hfql [{(submissions needle)
                   [:db/id
                    :dustingetz/email
                    {(:dustingetz/shirt-size . ::hf/options (shirt-sizes gender needle))
                     [:db/ident]}
                    {(:dustingetz/gender . ::hf/options #'genders) [(:db/ident . ::hf/as gender)]}]}
                  {(genders) [:db/ident]}]) ))
  % := :i-fail)

;;;;;;;;;;;;;;
;; DEFAULTS ;;
;;;;;;;;;;;;;;

;; DONE props on fn args
(tests
 "Input is defaulted"
 (p/run (! (let [default "alice"]
             (hfql {(submissions (needle . ::hf/default #'default)) [:db/id]}) 
             )))
 % := '{(user.gender-shirt-size/submissions needle) [#:db{:id 9}]})

;; TODO props on call ? Should it go on var meta?
;; order: args -> defaults -> call fn
#_(tests
 "call is defaulted"
 (p/run (! (let [default "alice"]
             (hfql {((submissions needle) . ::hf/default #'[default]) [:db/id]})
             )))
 % := '{(user.gender-shirt-size/submissions needle) [#:db{:id 9}]})

;; TODO hydrate defaults
#_(tests
 "Input is hydrated"
 (p/run (! (let [needle 9]
             (hfql {(submissions {needle [:db/id :dustingetz/email]}) [:db/id]})
             )
           ))
 % := '{(user.gender-shirt-size/submissions needle) {:db/id 9}})

;; TODO symbol as nav
;; TODO call as nav
;; TODO static link [[file:q6.cljc::"static link"][static link]] 
;; TODO link as quoted sym [[file:q6.cljc::"link as quoted sym"][link as quoted sym]] 
;; TODO templated link [[file:q6.cljc::"templated link"][templated link]] 

;; (rcf/enable! false)
