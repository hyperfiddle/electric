(ns hyperfiddle.hfql2.tests
  (:require
   [hyperfiddle.api :as hf]
   [hyperfiddle.hfql2 :refer [hfql]]
   [hyperfiddle.photon :as p]
   [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
   [datascript.core :as d]
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   #?(:clj [wip.orders-datascript :refer [orders order shirt-sizes one-order nav! schema]]))
  (:import [hyperfiddle.photon Pending]))

(comment
  (rcf/enable! true))

(p/defn TreeToRows [V]
  (new (p/Y. (p/fn [Rec]
               (p/fn [V]
                 (let [{::hf/keys [continuation columns render ] :as m}
                       (meta V)
                       _ (prn (:dbg/name m))
                       v (if render (render. V) (V.))]
                   (if-not continuation
                     [(p/fn [] [v])]
                     (cond
                       (fn? v)     (Rec. v)
                       (map? v)    (into [] cat (p/for [col columns]
                                                  (let [v' (get v col)
                                                        m' (meta v')]
                                                    (if (::hf/continuation m')
                                                      (into [(p/fn [] [col])] (Rec. v'))
                                                      [(p/fn [] [col (if-let [render (::hf/render m')]
                                                                       (render. v')
                                                                       (new v'))])]))))
                       (vector? v) (into [] cat (p/for [V v] (Rec. V)))
                       :else [(p/fn [] [v])]))))))
    V))

(p/defn Sequence [Vs] (p/fn [] (p/for [V Vs] (V.))))

(defmacro debug [& body]
  `(try ~@body
        (catch hyperfiddle.photon.Pending e# (throw e#))
        (catch missionary.Cancelled e# (throw e#))
        (catch Throwable e# (prn (ex-message e#)))))

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (hf/EdnRender. (hfql :db/id) ) ))))
  % := 9)

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (hf/EdnRender. (hfql [:db/id]) )))))
  % := {:db/id 9})

(p/def String-renderer (p/fn [V] (str (new V))))

(tests
  "hf/render"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (hf/EdnRender. (hfql (props :db/id {::hf/render String-renderer})) )))))
  % := "9")

(tests
  "hf/render inline"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (hf/EdnRender. (hfql (props :db/id {::hf/render (p/fn [V] (str (new V)))})))))))
  % := "9")

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (hf/EdnRender. (hfql [(props :db/id {::hf/render String-renderer})]))))))
  % := {:db/id "9"})

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9
                              hf/*schema* schema]
                      (hf/EdnRender. (hfql [{:order/gender [:db/ident]}]) )))))
  % := {:order/gender {:db/ident :order/female}})

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (debug (new (hfql (order "")) )) ))))
  % := 9)

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (debug (hf/EdnRender. (hfql {(order "") [:db/id]}) ))))))
  % := {`(order "") {:db/id 9}})

(tests
  "Two levels of nesting"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/*schema* schema]
                      (hf/EdnRender. (hfql {(order "") [{:order/shirt-size [:db/ident]}]})) ))))
  % := {`(order "") {:order/shirt-size {:db/ident :order/womens-large}}})

(tests
  "multiplicity many"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (debug (hf/EdnRender. (hfql {(orders "") [:db/id]}) ))))))
  % := {`(orders "") [{:db/id 9} {:db/id 10} {:db/id 11}]})

(tests
  (with (p/run (tap (binding [hf/db hf/*$*
                              hf/*nav!* nav!]
                      (hf/EdnRender. (hfql {(orders "") [(props :db/id {::hf/render String-renderer})]}))))))
  % := {`(orders "") [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})

(p/defn Throwing-renderer [V] (prn "BOOM") (throw (ex-info "I fail" {})))
(p/defn Ignoring-renderer [V]
  "ignored")

(tests
  (p/run (tap (binding [hf/db       hf/*$*
                        hf/*nav!*   nav!
                        hf/entity   9
                        hf/*schema* schema]
                (hf/EdnRender. (hfql [{(props :order/gender {::hf/render Ignoring-renderer})
                                       [(props :db/ident {::hf/render Throwing-renderer})]}]) ))))
  % := {:order/gender "ignored"} ; note it didn’t throw
  )

;; Insight: Let the renderer decide of the options' continuation.
(p/defn Select-option-renderer [V]
  (let [{::hf/keys [options continuation]} (meta V)]
    (into [:select {:value (hf/JoinAllTheTree. V)}]
      (p/for [e (new options)]
        [:option (if continuation (hf/JoinAllTheTree. (p/partial 1 continuation e)) e)]))))

(tests
  (with (p/run (tap (binding [hf/db       hf/*$*
                              hf/*nav!*   nav!
                              hf/entity   9
                              hf/*schema* schema]
                      (hf/EdnRender. (hfql [(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                     ::hf/options (shirt-sizes :order/female "")})]) )))))
  % := {:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]})

(tests
  "hf/options can continue with parent pullexpr"
  (with (p/run
          (tap (binding [hf/db       hf/*$*
                         hf/*nav!*   nav!
                         hf/entity   9
                         hf/*schema* schema]
                 (debug (hf/EdnRender. (hfql {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                        ::hf/options (shirt-sizes :order/female "")})
                                              [:db/ident]}) ))))))
  % := {:order/shirt-size [:select {:value #:db{:ident :order/womens-large}}
                           [:option #:db{:ident :order/womens-small}]
                           [:option #:db{:ident :order/womens-medium}]
                           [:option #:db{:ident :order/womens-large}]]})

(tests
  "Argument reference"
  (with (p/run (tap (binding [hf/db       hf/*$*
                              hf/*nav!*   nav!
                              hf/entity   9
                              hf/*schema* schema]
                      (hf/EdnRender. (hfql [{:order/gender [:db/ident]}
                                            (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                      ::hf/options (shirt-sizes db/ident "")})]) )))))
  % := {:order/gender     {:db/ident :order/female}
        :order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]}
  )

(tests
    "Argument reference under card n"
    (with (p/run (try (tap (binding [hf/db       hf/*$*
                                     hf/*nav!*   nav!
                                     hf/*schema* schema
                                     hf/entity   9]
                             (hf/EdnRender. (hfql {(orders "") [{:order/gender [:db/ident]}
                                                                (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                          ::hf/options (shirt-sizes db/ident "")})]}) )
                             ))
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
      (with (p/run (try (tap (binding [hf/db       hf/*$*
                                       hf/*nav!*   nav!
                                       hf/entity   9
                                       hf/*schema* schema]
                               (hf/EdnRender. (hfql {(orders needle1) [:order/email
                                                                       {:order/gender [(props :db/ident {::hf/as gender})]}
                                                                       {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                                  ::hf/options (shirt-sizes gender needle2)})
                                                                        [:db/ident]}]} ))))
                        (catch Pending _)))))
    % := {'(wip.orders-datascript/orders needle1)
          [{:order/shirt-size
            [:select
             {:value {:db/ident :order/womens-large}}
             [:option {:db/ident :order/womens-small}]],
            :order/email  "alice@example.com",
            :order/gender {:db/ident :order/female}}
           {:order/shirt-size
            [:select
             {:value {:db/ident :order/mens-large}}
             [:option {:db/ident :order/mens-small}]],
            :order/email  "bob@example.com",
            :order/gender {:db/ident :order/male}}
           {:order/shirt-size
            [:select
             {:value {:db/ident :order/mens-medium}}
             [:option {:db/ident :order/mens-small}]],
            :order/email  "charlie@example.com",
            :order/gender {:db/ident :order/male}}]})

(tests
    "free inputs"
    (with (p/run (try (tap (binding [hf/db       hf/*$*
                                     hf/*nav!*   nav!
                                     hf/*schema* schema]
                             (hf/EdnRender.
                               (hfql {(orders .) [{:order/gender [(props :db/ident {::hf/as gender})]}
                                                  {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                             ::hf/options (shirt-sizes gender .)})
                                                   [:db/ident]}]})) ) )
                      (catch Pending _))))
    % := {'(wip.orders-datascript/orders .)
          [{:order/gender {:db/ident :order/female},
            :order/shirt-size
            [:select
             {:value {:db/ident :order/womens-large}}
             [:option {:db/ident :order/womens-small}]
             [:option {:db/ident :order/womens-medium}]
             [:option {:db/ident :order/womens-large}]]}
           {:order/gender {:db/ident :order/male},
            :order/shirt-size
            [:select
             {:value {:db/ident :order/mens-large}}
             [:option {:db/ident :order/mens-small}]
             [:option {:db/ident :order/mens-medium}]
             [:option {:db/ident :order/mens-large}]]}
           {:order/gender {:db/ident :order/male},
            :order/shirt-size
            [:select
             {:value {:db/ident :order/mens-medium}}
             [:option {:db/ident :order/mens-small}]
             [:option {:db/ident :order/mens-medium}]
             [:option {:db/ident :order/mens-large}]]}]})

(defn suber-name [e]
  (first (str/split (:order/email (d/entity hf/*$* e)) #"@" 2)))

(s/fdef suber-name :ret string?)

(tests
  "function navigation"

  (with (p/run (try (tap
                      (binding [hf/db     hf/*$*
                                hf/*nav!* nav!]
                        (hf/EdnRender.
                          (hfql [hf/*$* hf/db]
                            {(orders "") [:db/id suber-name]})) ))
                    (catch Pending _))))
  % := `{(wip.orders-datascript/orders "") [{:db/id 9, suber-name "alice"} {:db/id 10, suber-name "bob"} {:db/id 11, suber-name "charlie"}]})

(def ^:dynamic *db*)

(s/fdef bound-order :args (s/cat :needle string?) :ret any?)

(defn bound-order [needle]
  #?(:clj (binding [hf/*$* *db*]
            (wip.orders-datascript/order needle))))

(tests
  "Binding conveyance"

  (with (p/run (try (tap
                      (binding [hf/db hf/*$*
                                hf/*nav!* nav!]
                        (hf/EdnRender. (hfql [*db* hf/db]
                                         {(bound-order "alice") [:db/id]})) ))
                    (catch Pending _))))
  % := '{(hyperfiddle.hfql2.tests/bound-order "alice") #:db{:id 9}})

(tests
  (with (p/run (tap (new (tap (with-meta (p/fn [] 1) {:contains :integer, :flow-type :constant}))))))
  (meta %) := {:contains :integer, :flow-type :constant}
  % := 1)
