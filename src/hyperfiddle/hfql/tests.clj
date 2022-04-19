(ns hyperfiddle.hfql.tests
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql :refer [hfql]]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [user.gender-shirt-size :refer [submissions submission shirt-sizes genders]]))

;; (rcf/enable!)
;; (rcf/enable! false)

(tests
 (p/run (! (binding [hf/entity 9]
             (hfql :db/id))))
 % := 9)

(tests
 (p/run (! (binding [hf/entity 9] (hfql [:db/id]))))
 % := 9)

(p/def string-renderer (p/fn [>v _props] (str (new >v))))

(tests
 "hf/render"
 (p/run (! (binding [hf/entity 9] (hfql (props :db/id {::hf/render string-renderer})))))
 % := "9")

(tests
 "hf/render inline"
 (p/run (! (binding [hf/entity 9] (hfql (props :db/id {::hf/render (p/fn [>v _props] (str (new >v)))})))))
 % := "9")

(tests
 (p/run (! (binding [hf/entity 9] (hfql [(props :db/id {::hf/render string-renderer})]))))
 % := "9")

(tests
 (p/run (binding [hf/entity 9] (! (hfql {:order/gender [:db/ident]}))))
 % := {:order/gender {:db/ident :order/female}})

(tests
 (p/run (binding [hf/entity 9] (! (hfql [{:order/gender [:db/ident]}]))))
 % := {:order/gender {:db/ident :order/female}})

(tests
 (p/run (! (hfql {(submission "") [:db/id]})))
 % := '{(user.gender-shirt-size/submission "") {:db/id 9}})

(tests
 "Two levels of nesting"
 (p/run (! (hfql {(submission "") [{:order/shirt-size [:db/ident]}]})))
 % := {'(user.gender-shirt-size/submission "") {:order/shirt-size {:db/ident :order/womens-large}}})

(tests
 "multiplicity many"
 (p/run (! (hfql {(submissions "" false) [:db/id]})))
 % := {'(user.gender-shirt-size/submissions "" false) [{:db/id 9} {:db/id 10} {:db/id 11}]})

(tests
 (p/run (! (hfql {(submissions "" false) [(props :db/id {::hf/render string-renderer})]})))
 % := {'(user.gender-shirt-size/submissions "" false) [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})



(defn fail []
  (throw (ex-info "I fail" {})))

(p/def throwing-renderer (p/fn [] (fail)))

(p/def ignoring-renderer (p/fn [] "ignored"))

(s/def :order/gender any?)

(tests
 (p/run (! (binding [hf/entity 9]
             (hfql [{(props :order/gender {::hf/render ignoring-renderer}) [(props :db/ident {::hf/render throwing-renderer})]}]))))
 % := {:order/gender "ignored"}
 ;; note it didn’t throw
 )

(p/defn select-option-renderer [>v props]
  (into [:select {:value (new hf/join-all (new >v))}]
        (p/for [e (new (::hf/options props))]
          [:option e])))

(tests
 (p/run (! (binding [hf/entity 9]
             (hfql (props :order/shirt-size {::hf/render  select-option-renderer
                                             ::hf/options (shirt-sizes :order/female "")})))))
 %
 := {:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]})


(tests
 (p/run (! (binding [hf/entity 9]
             (hfql {(props :order/shirt-size {::hf/render  select-option-renderer
                                              ::hf/options (shirt-sizes :order/female "")})
                    [:db/ident]}))))
 % := {:order/shirt-size [:select {:value #:db{:ident :order/womens-large}}
                          [:option #:db{:ident :order/womens-small}]
                          [:option #:db{:ident :order/womens-medium}]
                          [:option #:db{:ident :order/womens-large}]]})

;;;;;;;;;;;;;;;
;; ENV TESTS ;;
;;;;;;;;;;;;;;;

(tests
 (p/run (binding [hf/entity 9]
          (! (hfql [(props :order/shirt-size {::hf/options (shirt-sizes gender "")})
                    {:order/gender [(props :db/ident {::hf/as gender})]}]))))
 % := {:order/gender     #:db{:ident :order/female},
       :order/shirt-size 8})

(p/defn shirt-sizes-renderer [>v props] (new (::hf/options props)))

(tests
 (p/run (binding [hf/entity 9]
          (! (hfql [{:order/gender [(props :db/ident {::hf/as gender})]}
                    {(props :order/shirt-size {::hf/options (shirt-sizes gender "")
                                               ::hf/render shirt-sizes-renderer})
                     [:db/ident]}]))))
 % := {:order/gender     {:db/ident :order/female},
       :order/shirt-size [{:db/ident :order/womens-small}
                          {:db/ident :order/womens-medium}
                          {:db/ident :order/womens-large}]})

(tests
 "env under card-n"
 (p/run (binding [hf/entity 9]
          (! (hfql {(submissions "" false) [{:order/gender [(props :db/ident {::hf/as gender})]}
                                            {(props :order/shirt-size {::hf/options (shirt-sizes gender "")
                                                                       ::hf/render shirt-sizes-renderer})
                                             [:db/ident]}]}))))
 % := '{(user.gender-shirt-size/submissions "" false)
        [{:order/gender {:db/ident :order/female},
          :order/shirt-size
          [{:db/ident :order/womens-small}
           {:db/ident :order/womens-medium}
           {:db/ident :order/womens-large}]}
         {:order/gender {:db/ident :order/male},
          :order/shirt-size
          [{:db/ident :order/mens-small}
           {:db/ident :order/mens-medium}
           {:db/ident :order/mens-large}]}
         {:order/gender {:db/ident :order/male},
          :order/shirt-size
          [{:db/ident :order/mens-small}
           {:db/ident :order/mens-medium}
           {:db/ident :order/mens-large}]}]})
;;;;;;;;;;;;
;; NEEDLE ;;
;;;;;;;;;;;;

;; DONE
(tests
 "needle resolves from lexical env"
 (let [needle "alice"]
   (p/run (! (hfql {(submissions needle false) [:db/id]}))))
 % := '{(user.gender-shirt-size/submissions needle false) [#:db{:id 9}]})

;; DONE
(tests
 "Free input"
 (p/run (! (hfql {(submissions . .) [:db/id]})))
 % := '{(user.gender-shirt-size/submissions . .) [#:db{:id 9}
                                                #:db{:id 10}
                                                #:db{:id 11}]})

(p/defn render-typeahead [>v props]
  [:select {:value (new hf/join-all (new >v))}
   (p/for [e (new (::hf/options props))]
     [:option e])])

;; DONE
(tests
 "Two `needle` deep inputs. Not defined in lexical scope."
 (p/run (! (hfql {(submissions . .)
                  [:db/id
                   :order/email
                   {(props :order/shirt-size {::hf/render render-typeahead
                                              ::hf/options (shirt-sizes gender .)})
                    [:db/ident]}
                   {:order/gender [(props :db/ident {::hf/as gender})]}]})))
 % := '{(user.gender-shirt-size/submissions . .)
        [{:order/gender {:db/ident :order/female},
          :order/email "alice@example.com",
          :order/shirt-size
          [:select
           {:value {:db/ident :order/womens-large}}
           [[:option {:db/ident :order/womens-small}]
            [:option {:db/ident :order/womens-medium}]
            [:option {:db/ident :order/womens-large}]]],
          :db/id 9}
         {:order/gender {:db/ident :order/male},
          :order/email "bob@example.com",
          :order/shirt-size
          [:select
           {:value {:db/ident :order/mens-large}}
           [[:option {:db/ident :order/mens-small}]
            [:option {:db/ident :order/mens-medium}]
            [:option {:db/ident :order/mens-large}]]],
          :db/id 10}
         {:order/gender {:db/ident :order/male},
          :order/email "charlie@example.com",
          :order/shirt-size
          [:select
           {:value {:db/ident :order/mens-medium}}
           [[:option {:db/ident :order/mens-small}]
            [:option {:db/ident :order/mens-medium}]
            [:option {:db/ident :order/mens-large}]]],
          :db/id 11}]})

;;;;;;;;;;;;;;
;; DEFAULTS ;;
;;;;;;;;;;;;;;

;; DONE props on fn args
(p/defn default [%] (or % "alice"))
(tests
 "Input is defaulted"
 (p/run (! (hfql {(submissions ^{::hf/defaults default} . .) [:db/id]})))
 % := '{(user.gender-shirt-size/submissions . .) [#:db{:id 9}]})

(tests
 "call is defaulted"
 (p/run (! (hfql {(props (submissions . .) {::hf/defaults (p/fn [[needle]] [(or needle "alice")])}) [:db/id]})))
 % := '{(user.gender-shirt-size/submissions . .) [#:db{:id 9}]})

;; TODO hydrate defaults : put them in ::hf/inputs, it will compute when the client will sample the input
#_(tests
   "Input is hydrated"
   (p/run (! (let [needle 9]
               (hfql {(submissions {needle [:db/id :order/email]}) [:db/id]}))))
   % := '{(user.gender-shirt-size/submissions needle) {:db/id 9}})

;; (rcf/enable! false)
