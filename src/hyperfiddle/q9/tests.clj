(ns hyperfiddle.q9.tests
  (:require [clojure.spec.alpha :as s]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            ;; [hyperfiddle.q9 :refer [hfql]]
            [hyperfiddle.q9 :refer [hfql]]
            [hyperfiddle.q9.impl :as impl]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [user.gender-shirt-size :refer [submissions submission shirt-sizes genders]]))

;; (rcf/enable!)
;; (rcf/enable! false)

(tests
 (p/run (! (binding [hf/entity 9] (hfql :db/id) )))
 % := 9)

(tests
 (p/run (! (binding [hf/entity 9] (hfql [:db/id]) )))
 % := 9)

(p/def string-renderer (p/fn [>v _props] (str ~>v)))

(tests
 "hf/render"
 (p/run (! (binding [hf/entity 9] (hfql (props :db/id {::hf/render string-renderer})) )))
 % := "9")

(tests
 "hf/render inline"
 (p/run (! (binding [hf/entity 9] (hfql (props :db/id {::hf/render (p/fn [>v _props] (str ~>v))})) )))
 % := "9")

(tests
 (p/run (! (binding [hf/entity 9] (hfql [(props :db/id {::hf/render string-renderer})]) )))
 % := "9")

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
 (p/run (! (hfql {(submission "") [(props :dustingetz/email {::hf/render (p/fn [_ _] (let [[>e a >v] (first hf/context)]
                                                                                       [~>e a ~>v]))})]}) ))
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
 (p/run (! (hfql {(submissions "") [(props :db/id {::hf/render string-renderer})]}) ))
 % := {'(user.gender-shirt-size/submissions "") [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})



(defn fail []
  (throw (ex-info "I fail" {})))

(p/def throwing-renderer #'(fail))

(p/def ignoring-renderer #'"ignored")

(s/def :dustingetz/gender any?)

(tests
 (p/run (! (binding [hf/entity 9]
             (hfql [{(props :dustingetz/gender {::hf/render ignoring-renderer}) [(props :db/ident {::hf/render throwing-renderer})]}]) )))
 % := #:dustingetz{:gender "ignored"}
 ;; note it didn’t throw
 )

(p/defn select-option-renderer [>v props]
  (into [:select {:value (p/$ hf/join-all ~>v)}]
        (p/for [e ~(::hf/options props)]
          [:option e])))

(tests
 (p/run (! (binding [hf/entity 9]
             (hfql (props :dustingetz/shirt-size {::hf/render  select-option-renderer
                                                  ::hf/options (shirt-sizes :dustingetz/female "")})) 
             )))
 %
 := {:dustingetz/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]}
 )


(tests
 (p/run (! (binding [hf/entity 9]
             (hfql {(props :dustingetz/shirt-size {::hf/render  select-option-renderer
                                                   ::hf/options (shirt-sizes :dustingetz/female "")})
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
          (! (hfql [(props :dustingetz/shirt-size {::hf/options (shirt-sizes gender "")})
                    {:dustingetz/gender [(props :db/ident {::hf/as gender})]}]) 
             )))
 % := #:dustingetz{:gender     #:db{:ident :dustingetz/female},
                   :shirt-size 8})

(p/defn shirt-sizes-renderer [>v props] ~(::hf/options props))

(tests
 (p/run (binding [hf/entity 9]
          (! (hfql [{:dustingetz/gender [(props :db/ident {::hf/as gender})]}
                    {(props :dustingetz/shirt-size {::hf/options (shirt-sizes gender "")
                                                    ::hf/render shirt-sizes-renderer})
                     [:db/ident]}]) )))
 % := {:dustingetz/gender     {:db/ident :dustingetz/female},
       :dustingetz/shirt-size [{:db/ident :dustingetz/womens-small}
                               {:db/ident :dustingetz/womens-medium}
                               {:db/ident :dustingetz/womens-large}]})

(tests
 "env under card-n"
 (p/run (binding [hf/entity 9]
          (! (hfql {(submissions "") [{:dustingetz/gender [(props :db/ident {::hf/as gender})]}
                                      {(props :dustingetz/shirt-size {::hf/options (shirt-sizes gender "")
                                                                      ::hf/render shirt-sizes-renderer})
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

;; DONE
(tests
 "Free input"
 (p/run (! (hfql {(submissions .) [:db/id]})))
 % := '{(user.gender-shirt-size/submissions .) [#:db{:id 9}
                                                #:db{:id 10}
                                                #:db{:id 11}]})

(p/defn render-typeahead [>v props]
  [:select {:value (p/$ hf/join-all ~>v)}
   (p/for [e ~(::hf/options props)]
     [:option e])])

;; DONE
(tests
 "Two `needle` deep inputs. Not defined in lexical scope."
  (p/run (! (hfql {(submissions .)
                   [:db/id
                    :dustingetz/email
                    {(props :dustingetz/shirt-size {::hf/render render-typeahead
                                                ::hf/options (shirt-sizes gender .)})
                     [:db/ident]}
                    {:dustingetz/gender [(props :db/ident {::hf/as gender})]}]}) ))
  % := '{(user.gender-shirt-size/submissions .)
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

;;;;;;;;;;;;;;
;; DEFAULTS ;;
;;;;;;;;;;;;;;

;; DONE props on fn args
(p/defn default [%] (or % "alice"))
(tests
 "Input is defaulted"
 (p/run (! (hfql {(submissions ^{::hf/defaults default} .) [:db/id]})
             ))
 % := '{(user.gender-shirt-size/submissions .) [#:db{:id 9}]})

(tests
 "call is defaulted"
 (p/run (! (hfql {(props (submissions .) {::hf/defaults (p/fn [[needle]] [(or needle "alice")])}) [:db/id]}) ))
 % := '{(user.gender-shirt-size/submissions .) [#:db{:id 9}]})

;; TODO hydrate defaults : put them in ::hf/inputs, it will compute when the client will sample the input
#_(tests
 "Input is hydrated"
 (p/run (! (let [needle 9]
             (hfql {(submissions {needle [:db/id :dustingetz/email]}) [:db/id]})
             )
           ))
 % := '{(user.gender-shirt-size/submissions needle) {:db/id 9}})

;; (rcf/enable! false)
