(ns hyperfiddle.hfql.explorer-tests
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

;; Problems:
;; - Treeseqing an HFQL result implies dynamic scope is lost

(comment (rcf/enable! true))

(p/def TreeToRows hf/TreeToRows)

(defmacro debug [& body]
  `(try ~@body
        (catch hyperfiddle.photon.Pending e# (throw e#))
        (catch missionary.Cancelled e# (throw e#))
        (catch Throwable e# (prn (ex-message e#)))))

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (new (hf/Sequence. (TreeToRows. (hfql :db/id) ))) ))))
  % := [[9]])

(tests
  (with (p/run (debug  (tap (binding [hf/db     hf/*$*
                                      hf/*nav!* nav!
                                      hf/entity 9]
                              (new (hf/Sequence. (TreeToRows. (hfql [:db/id]) ))))))))
  % := [[:db/id 9]])

(p/def String-renderer (p/fn [V] (str (new V))))

(tests
  "hf/render"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (new (hf/Sequence. (TreeToRows. (hfql (props :db/id {::hf/render String-renderer})))) )))))
  % := [["9"]])

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (new (hf/Sequence. (TreeToRows. (hfql [(props :db/id {::hf/render String-renderer})]))))))))
  % := [[:db/id "9"]])

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9
                              hf/*schema* schema]
                      (new (hf/Sequence. (TreeToRows. (hfql [{:order/gender [:db/ident]}]))) )))))
  % := [[:order/gender] [:db/ident :order/female]])

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (debug (new (hf/Sequence. (TreeToRows. (hfql {(order "") [:db/id]})))) )))))
  % := [['(wip.orders-datascript/order "")] [:db/id 9]])

(tests
  "Two levels of nesting"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/*schema* schema]
                      (new (hf/Sequence. (TreeToRows. (hfql {(order "") [{:order/shirt-size [:db/ident]}]})))) ))))
  % := [['(wip.orders-datascript/order "")]
        [:order/shirt-size]
        [:db/ident :order/womens-large]])

(tests
  "multiplicity many"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (new (hf/Sequence. (TreeToRows. (hfql {(orders "") [:db/id]}) ))) ))))
  % := `[[(wip.orders-datascript/orders "")] [:db/id 9] [:db/id 10] [:db/id 11]])

(tests
  "skipping"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (new (hf/Sequence. (drop 2 (TreeToRows. (hfql {(orders "") [:db/id]}) )))) ))))
  % := `[[:db/id 10] [:db/id 11]])

(tests
  (with (p/run (tap (binding [hf/db hf/*$*
                              hf/*nav!* nav!]
                      (new (hf/Sequence. (TreeToRows. (hfql {(orders "") [(props :db/id {::hf/render String-renderer})]}))))))))
  % := `[[(wip.orders-datascript/orders "")] [:db/id "9"] [:db/id "10"] [:db/id "11"]])

(p/defn Throwing-renderer [V] (prn "BOOM") (throw (ex-info "I fail" {})))

(tests
  (p/run (tap (binding [hf/db       hf/*$*
                        hf/*nav!*   nav!
                        hf/entity   9
                        hf/*schema* schema]
                (new (hf/Sequence. (TreeToRows. (hfql [{(props :order/gender {::hf/render (p/fn [V] "ignored")})
                                                  [(props :db/ident {::hf/render Throwing-renderer})]}]))) ))))

  % := [[:order/gender "ignored"]]    ; note it didn’t throw
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
                      (new (hf/Sequence. (TreeToRows. (hfql [(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                    ::hf/options (shirt-sizes :order/female "")})]))) )))))
  % := [[:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]]])


(tests
  "hf/options can continue with parent pullexpr"
  (with (p/run
          (tap (binding [hf/db       hf/*$*
                         hf/*nav!*   nav!
                         hf/entity   9
                         hf/*schema* schema]
                 (new (hf/Sequence. (TreeToRows. (hfql {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                               ::hf/options (shirt-sizes :order/female "")})
                                                     [:db/ident]})))) ))))
  % := [[:order/shirt-size [:select {:value #:db{:ident :order/womens-large}}
                            [:option #:db{:ident :order/womens-small}]
                            [:option #:db{:ident :order/womens-medium}]
                            [:option #:db{:ident :order/womens-large}]]]])

(tests
  "Argument reference"
  (with (p/run (tap (binding [hf/db       hf/*$*
                              hf/*nav!*   nav!
                              hf/entity   9
                              hf/*schema* schema]
                      (new (hf/Sequence. (TreeToRows.
                                        (hfql [{:order/gender [:db/ident]}
                                               (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                         ::hf/options (shirt-sizes db/ident "")})])))) ))))
  % := [[:order/gender]
        [:db/ident :order/female]
        [:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]]]
  )

(tests
    "Argument reference under card n"
    (with (p/run (try (tap (binding [hf/db       hf/*$*
                                     hf/*nav!*   nav!
                                     hf/*schema* schema
                                     hf/entity   9]
                             (new (hf/Sequence. (TreeToRows.
                                               (hfql {(orders "") [{:order/gender [:db/ident]}
                                                                   (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                             ::hf/options (shirt-sizes db/ident "")})]})))) 
                             ))
                      (catch Pending _))))
    % := [['(wip.orders-datascript/orders "")]
          [:order/gender]
          [:db/ident :order/female]
          [:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]]
          [:order/gender]
          [:db/ident :order/male]
          [:order/shirt-size [:select {:value 5} [:option 3] [:option 4] [:option 5]]]
          [:order/gender]
          [:db/ident :order/male]
          [:order/shirt-size [:select {:value 4} [:option 3] [:option 4] [:option 5]]]])

(tests
  "lexical env"
  (let [needle1 ""
        needle2 "small"]
    (with (p/run (try (tap (binding [hf/db       hf/*$*
                                     hf/*nav!*   nav!
                                     hf/entity   9
                                     hf/*schema* schema]
                             (new (hf/Sequence. (TreeToRows.
                                               (hfql {(orders needle1) [:order/email
                                                                        {:order/gender [(props :db/ident {::hf/as gender})]}
                                                                        {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                                   ::hf/options (shirt-sizes gender needle2)})
                                                                         [:db/ident]}]} ))))))
                      (catch Pending _)))))
  % := [['(wip.orders-datascript/orders needle1)]
        [:order/email "alice@example.com"]
        [:order/gender]
        [:db/ident :order/female]
        [:order/shirt-size]
        [[:select
          {:value {:db/ident :order/womens-large}}
          [:option {:db/ident :order/womens-small}]]]
        [:order/email "bob@example.com"]
        [:order/gender]
        [:db/ident :order/male]
        [:order/shirt-size]
        [[:select
          {:value {:db/ident :order/mens-large}}
          [:option {:db/ident :order/mens-small}]]]
        [:order/email "charlie@example.com"]
        [:order/gender]
        [:db/ident :order/male]
        [:order/shirt-size]
        [[:select
          {:value {:db/ident :order/mens-medium}}
          [:option {:db/ident :order/mens-small}]]]])

(tests
  "free inputs"
  (with (p/run (try (tap (binding [hf/db       hf/*$*
                                   hf/*nav!*   nav!
                                   hf/*schema* schema]
                           (new (hf/Sequence. (TreeToRows.
                                             (hfql {(orders .) [{:order/gender [(props :db/ident {::hf/as gender})]}
                                                                {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                           ::hf/options (shirt-sizes gender .)})
                                                                 [:db/ident]}]})))) ) )
                    (catch Pending _))))
  % := [['(wip.orders-datascript/orders .)]
        [:order/gender]
        [:db/ident :order/female]
        [:order/shirt-size]
        [[:select
          {:value {:db/ident :order/womens-large}}
          [:option {:db/ident :order/womens-small}]
          [:option {:db/ident :order/womens-medium}]
          [:option {:db/ident :order/womens-large}]]]
        [:order/gender]
        [:db/ident :order/male]
        [:order/shirt-size]
        [[:select
          {:value {:db/ident :order/mens-large}}
          [:option {:db/ident :order/mens-small}]
          [:option {:db/ident :order/mens-medium}]
          [:option {:db/ident :order/mens-large}]]]
        [:order/gender]
        [:db/ident :order/male]
        [:order/shirt-size]
        [[:select
          {:value {:db/ident :order/mens-medium}}
          [:option {:db/ident :order/mens-small}]
          [:option {:db/ident :order/mens-medium}]
          [:option {:db/ident :order/mens-large}]]]])

(defn suber-name [e]
  (first (str/split (:order/email (d/entity hf/*$* e)) #"@" 2)))

(s/fdef suber-name :ret string?)

(tests
  "function navigation"

  (with (p/run (tap
                 (binding [hf/db     hf/*$*
                           hf/*nav!* nav!]
                   (new (hf/Sequence. (TreeToRows.
                                     (hfql [hf/*$* hf/db]
                                       {(orders "") [:db/id suber-name]}) ))) ))
          ))
  % := `[[(wip.orders-datascript/orders "")]
         [:db/id 9]
         [hyperfiddle.hfql.explorer-tests/suber-name "alice"]
         [:db/id 10]
         [hyperfiddle.hfql.explorer-tests/suber-name "bob"]
         [:db/id 11]
         [hyperfiddle.hfql.explorer-tests/suber-name "charlie"]])

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
                        (new (hf/Sequence. (TreeToRows. (hfql [*db* hf/db]
                                                       {(bound-order "alice") [:db/id]})))) ))
                    (catch Pending _))))
  % := `[[(bound-order "alice")] [:db/id 9]])



(comment


  ;; Problems:
  ;; - Treeseqing an HFQL result implies dynamic scope is lost


  (tests
    (with (p/run (tap (new (tap (with-meta (p/fn [] 1) {:contains :integer, :flow-type :constant}))))))
    (meta %) := {:contains :integer, :flow-type :constant}
    % := 1)
  )
