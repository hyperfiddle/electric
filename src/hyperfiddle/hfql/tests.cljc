(ns hyperfiddle.hfql.tests
  (:require
   [hyperfiddle.api :as hf]
   [hyperfiddle.hfql :refer [hfql]]
   [hyperfiddle.photon :as p]
   [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
   [datascript.core :as d]
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   #?(:clj [wip.orders-datascript :refer [orders order shirt-sizes one-order nav! schema]]))
  (:import [hyperfiddle.photon Pending]))

(comment
  (rcf/enable! true))

(defmacro debug [& body]
  `(try ~@body
        (catch hyperfiddle.photon.Pending e# (throw e#))
        (catch missionary.Cancelled e# (throw e#))
        (catch Throwable e# (prn (type e#) (ex-message e#) (ex-data e#)) (throw e#))))

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
                      (debug (hf/EdnRender. (hfql [:db/id]) ))))))
  % := {:db/id 9})

(p/def String-renderer (p/fn [{::hf/keys [Value]}] (str (new Value))))

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
                      (hf/EdnRender. (hfql (props :db/id {::hf/render (p/fn [{::hf/keys [Value]}] (str (new Value)))})))))))
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
                      (debug (hf/EdnRender. (hfql [{:order/gender [:db/ident]}]) ))))))
  % := {:order/gender {:db/ident :order/female}})

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (debug (hf/EdnRender. (hfql (order "")) )) ))))
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

(p/defn Throwing-renderer [ctx] (prn "BOOM") (throw (ex-info "I fail" {})))
(p/defn Ignoring-renderer [ctx] "ignored")

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
(p/defn Select-option-renderer [{::hf/keys [Value options continuation] :as ctx}]
  (into [:select {:value (hf/JoinAllTheTree. ctx)}]
    (p/for [e (new options)]
      [:option (if continuation (hf/JoinAllTheTree. (new continuation e)) e)])))

(tests
  (with (p/run (tap (binding [hf/db       hf/*$*
                              hf/*nav!*   nav!
                              hf/entity   9
                              hf/*schema* schema]
                      (debug (hf/EdnRender. (hfql [(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                             ::hf/options (shirt-sizes :order/female "")})]) ))))))
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
  % := '{(hyperfiddle.hfql.tests/bound-order "alice") #:db{:id 9}})

(tests
  (with (p/run (tap (new (tap (with-meta (p/fn [] 1) {:contains :integer, :flow-type :constant}))))))
  (meta %) := {:contains :integer, :flow-type :constant}
  % := 1)


(tests
  "Static Link on attribute"
  (with (p/run (tap (debug (-> (hfql (props :db/id {::hf/link [:home]}))
                             (::hf/link)
                             (new))))))
  % := [:home])

(tests
  "Templated Link"
  (with (p/run (tap (binding [hf/db hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (debug (-> (hfql [:db/id
                                        (props :order/email {::hf/link [:link db/id]})])
                               ::hf/values
                               (get 1)
                               (::hf/link)
                               (new)))))))
  % := [:link 9])

(p/defn Expr [] (hfql (props :order/email {::hf/link [:link %]})))

(tests
  "Templated Link with % ref"
  (with (p/run (tap (binding [hf/db hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (debug (-> (Expr.)
                               (::hf/link)
                               (new)))))))
  % := [:link 9])

(tests
  "Self referencing link"
  (with (p/run (tap (binding [hf/db hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (debug (-> (hfql (props :db/id {::hf/link [:link db/id]}))
                               (::hf/link)
                               (new)))))))
  % := [:link 9])


(comment
  (tests
    "Deep referencing link"
    (with (p/run (tap (binding [hf/db hf/*$*
                                hf/*nav!* nav!
                                hf/entity 9]
                        (debug (-> (hfql (props :db/id {::hf/link [:link [:db/id db/id]]}))
                                 (::hf/link)
                                 (new)))))))
    % := '(:link 9)))



(comment

  (hyperfiddle.hfql.impl/graph '(props :db/id {::hf/link '(:link db/id)}))

  (hfql (props :db/id {::hf/link '(:link db/id)}))
  (hyperfiddle.hfql.impl/graph '[:db/id
                                  (props :order/email {::hf/link '(:link db/id)})])

  )

(defn foo [a] a)

(s/fdef foo :args (s/cat :a string?) :ret string?)

(tests
  "default on fn arg" ; only for gray inputs
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (hf/JoinAllTheTree. (hfql (foo (or nil "ORed")) ; TODO allow referencing lexical scope from nested sexprs
                                            ))))))
  % := "ORed")

(p/defn Default [a]
  ;; TODO Default fn should see hf context (e.g. injected from dynamic scope.
  (or a "defaulted from photon"))

(tests
  "default of fn argument" ; only for gray inputs
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (debug (hf/JoinAllTheTree. (hfql (foo (Default. nil)))  ) ; TODO allow referencing lexical scope from nested sexprs
                        )))))
  % := "defaulted from photon")

(comment
  ;; TODO some default logic requires all arguments:
  (defn default [eid nom] [eid (if (and eid (empty? nom)) (suber-name eid) nom)])
  )


(tests
  "::hf/defaults"
  ;; Previous tests shows ::hf/default only exists because we cannot detect
  ;; lexical references in nested sexprs.
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 12]
                      (debug (hf/JoinAllTheTree. (hfql [:db/id
                                                        :order/email
                                                        (foo (props order/email {::hf/default (p/fn [a] (or a "defaulted"))}))])  )
                        )))))
  % := {:db/id 12,
        :order/email nil,
        '(hyperfiddle.hfql.tests/foo order/email) "defaulted"})
