(ns hyperfiddle.hfql-test
  (:require
   [hyperfiddle.api :as hf]
   [hyperfiddle.hfql :refer [hfql]]
   [hyperfiddle.photon :as p]
   [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
   [clojure.spec.alpha :as s]
   #?(:clj [wip.orders-datascript :refer [orders order shirt-sizes one-order]]))
  (:import [hyperfiddle.photon Pending]))

(tests
 (with (p/run (tap (binding [hf/db hf/*$*
                             hf/entity 9]
                     (hfql :db/id) ))))
 % := 9)

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/entity 9]
                     (hfql [:db/id]) ))))
  % := {:db/id 9})

(p/def string-renderer (p/fn [>v _props] (str (new >v))))

(tests
 "hf/render"
 (with (p/run (tap (binding [hf/db     hf/*$*
                             hf/entity 9]
                     (hfql (props :db/id {::hf/render string-renderer})) ))))
 % := "9")

(tests
 "hf/render inline"
 (with (p/run (tap (binding [hf/db     hf/*$*
                             hf/entity 9]
                     (hfql (props :db/id {::hf/render (p/fn [>v _props] (str (new >v)))}))))))
 % := "9")

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/entity 9]
                      (hfql [(props :db/id {::hf/render string-renderer})])))))
  % := {:db/id "9"})

(tests
  (with (p/run (binding [hf/db     hf/*$*
                         hf/entity 9]
                 (tap (hfql {:order/gender [:db/ident]}) ))))
  % := {:order/gender {:db/ident :order/female}})

(tests
  (with (p/run (binding [hf/db     hf/*$*
                         hf/entity 9]
                 (tap (hfql [{:order/gender [:db/ident]}])))))
  % := {:order/gender {:db/ident :order/female}})

(tests
  (with (p/run (tap (binding [hf/db hf/*$*]
                      (hfql {(order "") [:db/id]})))))
  % := {`(order "") {:db/id 9}})

(tests
 "Two levels of nesting"
 (with (p/run (tap (binding [hf/db hf/*$*]
                     (hfql {(order "") [{:order/shirt-size [:db/ident]}]})))))
 % := {`(order "") {:order/shirt-size {:db/ident :order/womens-large}}})

(tests
 "multiplicity many"
 (with (p/run (tap (binding [hf/db hf/*$*]
                     (hfql {(orders "") [:db/id]})) )))
 % := {`(orders "") [{:db/id 9} {:db/id 10} {:db/id 11}]})

(tests
  (with (p/run (tap (binding [hf/db hf/*$*]
                      (hfql {(orders "") [(props :db/id {::hf/render string-renderer})]})))))
  % := {`(orders "") [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})

(p/defn throwing-renderer [V props] (throw (ex-info "I fail" {})))
(p/defn ignoring-renderer [V props] "ignored")

(tests
  (p/run (tap (binding [hf/db     hf/*$*
                        hf/entity 9]
                (hfql [{(props :order/gender {::hf/render ignoring-renderer}) [(props :db/ident {::hf/render throwing-renderer})]}]))))
  % := {:order/gender "ignored"} ; note it didn’t throw
  )


(p/defn Select-option-renderer [>v props]
  (into [:select {:value (hf/Data. >v)}]
    (p/for [e (binding [hf/bypass-renderer true] (new (::hf/options props)))]
      [:option e])))

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/entity 9]
                      (hfql [(props :order/shirt-size {::hf/render  Select-option-renderer
                                                       ::hf/options (shirt-sizes :order/female "")})]) ))))
  % := {:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]})

(tests
  "hf/options inherit parent pullexpr"
  (with (p/run
          (tap (binding [hf/db     hf/*$*
                         hf/entity 9]
                 (hfql [{(props :order/shirt-size {::hf/render  Select-option-renderer
                                                   ::hf/options (shirt-sizes :order/female "")})
                         [:db/ident]}]) 
                 ))))
  % := {:order/shirt-size [:select {:value #:db{:ident :order/womens-large}}
                           [:option #:db{:ident :order/womens-small}]
                           [:option #:db{:ident :order/womens-medium}]
                           [:option #:db{:ident :order/womens-large}]]})

(tests
  "Argument reference"
  (with (p/run (try (tap (binding [hf/db     hf/*$*
                                   hf/entity 9]
                           (hfql [{:order/gender [:db/ident]}
                                  (props :order/shirt-size {::hf/render  Select-option-renderer
                                                            ::hf/options (shirt-sizes db/ident "")})]) ))
                    (catch Pending _))))
  ;; (prn %) := _
  % := {:order/gender     {:db/ident :order/female}
        :order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]})


(tests
  "Argument reference under card n"
  (with (p/run (try (tap (binding [hf/db     hf/*$*
                                   hf/entity 9]
                           (hfql {(orders "") [{:order/gender [:db/ident]}
                                               (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                         ::hf/options (shirt-sizes db/ident "")})]}) ))
                    (catch Pending _))))
  % := {`(orders "")
        [{:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]],
          :order/gender     {:db/ident :order/female}}
         {:order/shirt-size [:select {:value 5} [:option 3] [:option 4] [:option 5]],
          :order/gender     {:db/ident :order/male}}
         {:order/shirt-size [:select {:value 4} [:option 3] [:option 4] [:option 5]],
          :order/gender     {:db/ident :order/male}}]})

(tests
  "Argument reference under card n"
  (with (p/run (try (tap (binding [hf/db     hf/*$*
                                   hf/entity 9]
                           (hfql {(orders "") [{:order/gender [:db/ident]}
                                               (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                         ::hf/options (shirt-sizes db/ident "")})]}) ))
                    (catch Pending _))))
  % := {`(orders "")
        [{:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]],
          :order/gender     {:db/ident :order/female}}
         {:order/shirt-size [:select {:value 5} [:option 3] [:option 4] [:option 5]],
          :order/gender     {:db/ident :order/male}}
         {:order/shirt-size [:select {:value 4} [:option 3] [:option 4] [:option 5]],
          :order/gender     {:db/ident :order/male}}]})

(tests
  "lexical env"
  (let [needle1 ""
        needle2 "small"]
    (with (p/run (try (tap (binding [hf/db     hf/*$*
                                     hf/entity 9]
                             (hfql {(orders needle1) [:order/email
                                                      {:order/gender [(props :db/ident {::hf/as gender})]}
                                                      {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                 ::hf/options (shirt-sizes gender needle2)})
                                                       [:db/ident]}]} )))
                      (catch Pending _)))))
  % := {`(orders ~'needle1)
        [{:order/shirt-size
          [:select
           {:value {:db/ident :order/womens-large}}
           [:option {:db/ident :order/womens-small}]],
          :order/gender {:db/ident :order/female},
          :order/email  "alice@example.com"}
         {:order/shirt-size
          [:select
           {:value {:db/ident :order/mens-large}}
           [:option {:db/ident :order/mens-small}]],
          :order/gender {:db/ident :order/male},
          :order/email  "bob@example.com"}
         {:order/shirt-size
          [:select
           {:value {:db/ident :order/mens-medium}}
           [:option {:db/ident :order/mens-small}]],
          :order/gender {:db/ident :order/male},
          :order/email  "charlie@example.com"}]})

(tests
  "free inputs"
  (with (p/run (try (tap (binding [hf/db hf/*$*]
                           (hfql {(orders .) [{:order/gender [(props :db/ident {::hf/as gender})]}
                                              {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                         ::hf/options (shirt-sizes gender .)})
                                               [:db/ident]}]})) )
                    (catch Pending _))))
  % := {`(orders .)
         [#:order{:shirt-size
                  [:select
                   {:value #:db{:ident :order/womens-large}}
                   [:option #:db{:ident :order/womens-small}]
                   [:option #:db{:ident :order/womens-medium}]
                   [:option #:db{:ident :order/womens-large}]],
                  :gender #:db{:ident :order/female}}
          #:order{:shirt-size
                  [:select
                   {:value #:db{:ident :order/mens-large}}
                   [:option #:db{:ident :order/mens-small}]
                   [:option #:db{:ident :order/mens-medium}]
                   [:option #:db{:ident :order/mens-large}]],
                  :gender #:db{:ident :order/male}}
          #:order{:shirt-size
                  [:select
                   {:value #:db{:ident :order/mens-medium}}
                   [:option #:db{:ident :order/mens-small}]
                   [:option #:db{:ident :order/mens-medium}]
                   [:option #:db{:ident :order/mens-large}]],
                  :gender #:db{:ident :order/male}}]})


(def ^:dynamic *db*)

(s/fdef bound-order :args (s/cat :needle string?) :ret any?)

(defn bound-order [needle]
  #?(:clj (binding [hf/*$* *db*]
            (wip.orders-datascript/order needle))))

(tests
  "Binding conveyance"

  (with (p/run (try (tap
                      (binding [hf/db hf/*$*]
                        (hfql [*db* hf/db]
                          {(bound-order "alice") [:db/id]}) ))
                    (catch Pending _))))
  % := '{(hyperfiddle.hfql-test/bound-order "alice") #:db{:id 9}})
