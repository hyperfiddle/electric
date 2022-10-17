(ns hyperfiddle.hfql-test
  (:require
   [hyperfiddle.api :as hf]
   [hyperfiddle.photon :as p]
   [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
   [hyperfiddle.queries-test :refer [orders order shirt-sizes one-order]]))

(tests
 (with (p/run (tap (binding [hf/entity 9]
                     (hf/hfql :db/id) ))))
 % := 9)

(tests
 (with (p/run (tap (binding [hf/entity 9] (hf/hfql [:db/id]) ))))
 % := {:db/id 9})

(p/def string-renderer (p/fn [>v _props] (str (new >v))))

(tests
 "hf/render"
 (with (p/run (tap (binding [hf/entity 9] (hf/hfql (props :db/id {::hf/render string-renderer})) ))))
 % := "9")

(tests
 "hf/render inline"
 (with (p/run (tap (binding [hf/entity 9] (hf/hfql (props :db/id {::hf/render (p/fn [>v _props] (str (new >v)))}))))))
 % := "9")

(tests
 (with (p/run (tap (binding [hf/entity 9] (hf/hfql [(props :db/id {::hf/render string-renderer})])))))
 % := {:db/id "9"})

(tests
 (with (p/run (binding [hf/entity 9] (tap (hf/hfql {:order/gender [:db/ident]}) ))))
 % := {:order/gender {:db/ident :order/female}})

(tests
 (with (p/run (binding [hf/entity 9] (tap (hf/hfql [{:order/gender [:db/ident]}])))))
 % := {:order/gender {:db/ident :order/female}})

(tests
 (with (p/run (tap (hf/hfql {(order "") [:db/id]}))))
 % := '{(hyperfiddle.queries-test/order "") {:db/id 9}})

(tests
 "Two levels of nesting"
 (with (p/run (tap (hf/hfql {(order "") [{:order/shirt-size [:db/ident]}]}))))
 % := {'(hyperfiddle.queries-test/order "") {:order/shirt-size {:db/ident :order/womens-large}}})

(tests
 "multiplicity many"
 (with (p/run (tap (hf/hfql {(orders "") [:db/id]}) )))
 % := {'(hyperfiddle.queries-test/orders "") [{:db/id 9} {:db/id 10} {:db/id 11}]})

(tests
 (with (p/run (tap (hf/hfql {(orders "") [(props :db/id {::hf/render string-renderer})]}))))
 % := {'(hyperfiddle.queries-test/orders "") [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})

(p/defn throwing-renderer [V props] (throw (ex-info "I fail" {})))
(p/defn ignoring-renderer [V props] "ignored")

(tests
  (p/run (tap (binding [hf/entity 9]
                (hf/hfql [{(props :order/gender {::hf/render ignoring-renderer}) [(props :db/ident {::hf/render throwing-renderer})]}]))))
  % := {:order/gender "ignored"} ; note it didn’t throw
  )


(p/defn Select-option-renderer [>v props]
  (into [:select {:value (hf/Data. >v)}]
    (p/for [e (binding [hf/bypass-renderer true] (new (::hf/options props)))]
      [:option e])))

(tests
  (with (p/run (tap (binding [hf/entity 9]
                      (hf/hfql [(props :order/shirt-size {::hf/render  Select-option-renderer
                                                       ::hf/options (shirt-sizes :order/female "")})]) ))))
  % := {:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]})

(tests
  "hf/options inherit parent pullexpr"
  (with (p/run
          (tap (binding [hf/entity 9]
                 (hf/hfql [{(props :order/shirt-size {::hf/render  Select-option-renderer
                                                   ::hf/options (shirt-sizes :order/female "")})
                         [:db/ident]}]) 
                 ))))
  % := {:order/shirt-size [:select {:value #:db{:ident :order/womens-large}}
                           [:option #:db{:ident :order/womens-small}]
                           [:option #:db{:ident :order/womens-medium}]
                           [:option #:db{:ident :order/womens-large}]]})

(tests
  "Argument reference"
  (with (p/run (tap (binding [hf/entity 9]
                      (hf/hfql [{:order/gender [:db/ident]}
                             (props :order/shirt-size {::hf/render  Select-option-renderer
                                                       ::hf/options (shirt-sizes db/ident "")})]) ))))
  % := {:order/gender {:db/ident :order/female}
        :order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]})


(tests
  "Argument reference under card n"
  (with (p/run (tap (binding [hf/entity 9]
                      (hf/hfql {(orders "") [{:order/gender [:db/ident]}
                                          (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                    ::hf/options (shirt-sizes db/ident "")})]}) ))))
  % := '{(hyperfiddle.queries-test/orders "")
         [{:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]],
           :order/gender {:db/ident :order/female}}
          {:order/shirt-size [:select {:value 5} [:option 3] [:option 4] [:option 5]],
           :order/gender {:db/ident :order/male}}
          {:order/shirt-size [:select {:value 4} [:option 3] [:option 4] [:option 5]],
           :order/gender {:db/ident :order/male}}]})

(tests
  "Argument reference under card n"
  (with (p/run (tap (binding [hf/entity 9]
                      (hf/hfql {(orders "") [{:order/gender [:db/ident]}
                                          (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                    ::hf/options (shirt-sizes db/ident "")})]}) ))))
  % := '{(hyperfiddle.queries-test/orders "")
         [{:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]],
           :order/gender {:db/ident :order/female}}
          {:order/shirt-size [:select {:value 5} [:option 3] [:option 4] [:option 5]],
           :order/gender {:db/ident :order/male}}
          {:order/shirt-size [:select {:value 4} [:option 3] [:option 4] [:option 5]],
           :order/gender {:db/ident :order/male}}]})

(tests
  "lexical env"
  (let [needle1  ""
        needle2  "small"]
    (with (p/run (tap (binding [hf/entity 9]
                        (hf/hfql {(orders needle1) [:order/email
                                                 {:order/gender [(props :db/ident {::hf/as gender})]}
                                                 {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                            ::hf/options (shirt-sizes gender needle2)})
                                                  [:db/ident]}]}) )))))
  % := '{(hyperfiddle.queries-test/orders needle1)
         [{:order/shirt-size
           [:select
            {:value {:db/ident :order/womens-large}}
            [:option {:db/ident :order/womens-small}]],
           :order/gender {:db/ident :order/female},
           :order/email "alice@example.com"}
          {:order/shirt-size
           [:select
            {:value {:db/ident :order/mens-large}}
            [:option {:db/ident :order/mens-small}]],
           :order/gender {:db/ident :order/male},
           :order/email "bob@example.com"}
          {:order/shirt-size
           [:select
            {:value {:db/ident :order/mens-medium}}
            [:option {:db/ident :order/mens-small}]],
           :order/gender {:db/ident :order/male},
           :order/email "charlie@example.com"}]})

(tests
  "free inputs"
  (with (p/run (tap (hf/hfql {(orders .) [
                                       {:order/gender [(props :db/ident {::hf/as gender})]}
                                       {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                  ::hf/options (shirt-sizes gender .)})
                                        [:db/ident]}]}) )))
  % := '{(hyperfiddle.queries-test/orders .)
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


