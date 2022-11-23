(ns hyperfiddle.hfql.explorer-tests
  (:require
   [hyperfiddle.api :as hf :refer [hfql]]
   [hyperfiddle.hfql.explorer :as ex :refer [#_TreeToExplorer Sequence]]
   [hyperfiddle.hfql :as hfql]
   [hyperfiddle.photon :as p]
   [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
   [datascript.core :as d]
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   #?(:clj [wip.orders-datascript :refer [orders order shirt-sizes one-order nav! schema]]))
  (:import [hyperfiddle.photon Pending]))

(comment (rcf/enable! true))

(defmacro debug [& body]
  `(try ~@body
        (catch hyperfiddle.photon.Pending e# (throw e#))
        (catch missionary.Cancelled e# (throw e#))
        (catch Throwable e# (prn (ex-message e#)))))

;; A renderer mappping and HFQL tree to an explorer compatible structure:
;; Flow[[Flow[depth, Flow[[Flow[col], …]], …]]

(p/def Rec)

(p/defn FormsTransposedToRows [{::hf/keys [keys] :as ctx} v depth]
  (ex/inject-rows (into [(p/fn [] [depth (p/fn [] (p/for [k keys] (p/fn [] k)))])]
                 cat (p/for [ctx v]
                       [(p/fn [] [depth (p/fn []
                                          (let [{::hf/keys [keys values]} ctx]
                                            (p/for-by first [[k ctx] (mapv vector keys values)]
                                              (p/fn []
                                                (let [{::hf/keys [render summarize]} ctx]
                                                  (cond
                                                    render  (render. ctx)
                                                    :else   (let [value (hf/JoinAllTheTree. ctx)]
                                                              (if summarize
                                                                (summarize. value)
                                                                value))))))) )])]))))

(p/defn HandleCardMany [{::hf/keys [type] :as ctx} v depth]
  (case type
    ::hf/leaf (into [] cat (p/for [ctx v] (Rec. ctx (inc depth))))
    (FormsTransposedToRows. ctx v depth)))

(p/defn FormLabel [{::hf/keys [attribute] :as ctx} depth]
  [(p/fn [] [depth (p/fn [] [(p/fn [] attribute)])])])

(p/defn TreeToExplorer [ctx]
  (binding [Rec (p/fn [{::hf/keys [type render keys values Value] :as ctx} depth]
                  (if render
                    (let [v (render. (assoc ctx ::depth depth))]
                      (if (ex/rows? v)
                        v
                        [(ex/capture [Rec] [depth (p/fn [] [(p/fn [] v)])])]))
                    (case type
                      ::hf/leaf (ex/rows (ex/row depth [(ex/col (Value.))]))
                      ::hf/keys (into [] cat (p/for-by first [[k ctx] (mapv vector keys values)]
                                               (if (= ::hf/leaf (::hf/type ctx))
                                                 [(ex/capture [Rec] (ex/row depth [(ex/col k)
                                                                                   (ex/col (if-let [render (::hf/render ctx)]
                                                                                             (render. ctx)
                                                                                             (new (::hf/Value ctx))))]))]
                                                 (into (FormLabel. ctx depth)
                                                   (Rec. ctx (inc depth))))))
                      (let [v (Value.)]
                        (cond (vector? v) (HandleCardMany. ctx v depth) ; card many
                              (map? v)    (Rec. v depth)                ; card one
                              :else       (throw (ex-info "unreachable" {:value v})))))))]
    (new Rec ctx 0)))


(p/defn Rows [hfql] (p/for [[depth cols] (new (Sequence. (TreeToExplorer. hfql)))]
                      [depth (new (Sequence. (new cols)))]))

(p/defn Window [skip width hfql]
  (p/for [[depth cols] (new (Sequence. (take width (drop skip (TreeToExplorer. hfql)))))]
    [depth (new (Sequence. (new cols)))]))

;; -----------------------------------------------------------------------------

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (Rows. (hfql/hfql :db/id))   ))))
  % := [[0 [9]]]
  )

(tests
  (with (p/run (debug  (tap (binding [hf/db     hf/*$*
                                      hf/*nav!* nav!
                                      hf/entity 9]
                              (Rows. (hfql [:db/id]) ))))))
  % := [[0 [:db/id 9]]])

(p/def String-renderer (p/fn [{::hf/keys [Value] :as ctx}] (str (Value.))))

(tests
  "hf/render"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (debug (Rows. (hfql (props :db/id {::hf/render String-renderer})))) ))))
  % := [[0 ["9"]]])

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9]
                      (Rows. (hfql [(props :db/id {::hf/render String-renderer})]))))))
  % := [[0 [:db/id "9"]]])

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/entity 9
                              hf/*schema* schema]
                      (Rows. (hfql [{:order/gender [:db/ident]}])) ))))
  % := [[0 [:order/gender]]
        [1 [:db/ident :order/female]]])

(tests
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (Rows. (hfql {(order "") [:db/id]})) ))))
  % := [[0 ['(wip.orders-datascript/order "")]]
        [1 [:db/id 9]]])

(tests
  "Two levels of nesting"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!
                              hf/*schema* schema]
                      (Rows. (hfql {(order "") [{:order/shirt-size [:db/ident]}]})) ))))
  % := [[0 ['(wip.orders-datascript/order "")]]
        [1 [:order/shirt-size]]
        [2 [:db/ident :order/womens-large]]])

(tests
  "multiplicity many"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (Rows. (hfql {(orders "") [:db/id]}) ) ))))
  % := `[[0 [(wip.orders-datascript/orders "")]]
         [1 [:db/id]]
         [1 [9]]
         [1 [10]]
         [1 [11]]])

(tests
  "skipping"
  (with (p/run (tap (binding [hf/db     hf/*$*
                              hf/*nav!* nav!]
                      (Window. 1 2 (hfql {(orders "") [:db/id]}) ) ))))
  % := [[1 [:db/id]]
        [1 [9]]])

(tests
  (with (p/run (tap (binding [hf/db hf/*$*
                              hf/*nav!* nav!]
                      (Rows. (hfql {(orders "") [(props :db/id {::hf/render String-renderer})]}))))))
  % := `[[0 [(wip.orders-datascript/orders "")]]
         [1 [:db/id]]
         [1 ["9"]]
         [1 ["10"]]
         [1 ["11"]]])

(p/defn Throwing-renderer [V] (prn "BOOM") (throw (ex-info "I fail" {})))

(tests
  (p/run (tap (binding [hf/db       hf/*$*
                        hf/*nav!*   nav!
                        hf/entity   9
                        hf/*schema* schema]
                (Rows. (hfql [{(props :order/gender {::hf/render (p/fn [_] "ignored")})
                               [(props :db/ident {::hf/render Throwing-renderer})]}])) )))
  % := [[0 [:order/gender]]             ; TODO "ignored" should render inline next to the label
        [1 ["ignored"]]]                ; note it didn’t throw
  )

;; Insight: Let the renderer decide of the options' continuation.
(p/defn Select-option-renderer [{::hf/keys [options continuation] :as ctx}]
  (into [:select {:value (hf/JoinAllTheTree. ctx)}]
    (p/for [e (new options)]
      [:option (if continuation (hf/JoinAllTheTree. (continuation. e)) e)])))

(tests
  (with (p/run (tap (binding [hf/db       hf/*$*
                              hf/*nav!*   nav!
                              hf/entity   9
                              hf/*schema* schema]
                      (Rows. (hfql [(props :order/shirt-size {::hf/render  Select-option-renderer
                                                              ::hf/options (shirt-sizes :order/female "")})])) ))))
  % := [[0 [:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]]]])

(tests
  "hf/options can continue with parent pullexpr"
  (with (p/run
          (tap (binding [hf/db       hf/*$*
                         hf/*nav!*   nav!
                         hf/entity   9
                         hf/*schema* schema]
                 (Rows. (hfql {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                         ::hf/options (shirt-sizes :order/female "")})
                               [:db/ident]})) ))))
  % := [[0 [:order/shirt-size]]
        [1 [[:select {:value #:db{:ident :order/womens-large}}
             [:option #:db{:ident :order/womens-small}]
             [:option #:db{:ident :order/womens-medium}]
             [:option #:db{:ident :order/womens-large}]]]]])

(tests
  "Argument reference"
  (with (p/run (tap (binding [hf/db       hf/*$*
                              hf/*nav!*   nav!
                              hf/entity   9
                              hf/*schema* schema]
                      (Rows. (hfql [{:order/gender [:db/ident]}
                                    (props :order/shirt-size {::hf/render  Select-option-renderer
                                                              ::hf/options (shirt-sizes db/ident "")})])) ))))
  % := [[0 [:order/gender]]
        [1 [:db/ident :order/female]]
        [0 [:order/shirt-size [:select {:value 8} [:option 6] [:option 7] [:option 8]]]]]
  )

(tests
  "Argument reference under card n"
  (with (p/run (try (tap (binding [hf/db       hf/*$*
                                   hf/*nav!*   nav!
                                   hf/*schema* schema
                                   hf/entity   9]
                           (Rows. (hfql {(orders "") [{:order/gender [:db/ident]}
                                                      (props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                ::hf/options (shirt-sizes db/ident "")})]}))))
                    (catch Pending _))))
  % := [[0 ['(wip.orders-datascript/orders "")]]
        [1 [:order/gender :order/shirt-size]]
        [1 [{:db/ident :order/female}
            [:select {:value 8} [:option 6] [:option 7] [:option 8]]]]
        [1 [{:db/ident :order/male}
            [:select {:value 5} [:option 3] [:option 4] [:option 5]]]]
        [1 [{:db/ident :order/male}
            [:select {:value 4} [:option 3] [:option 4] [:option 5]]]]])

(tests
  "lexical env"
  (let [needle1 "li"                  ; aLIce, charLIe, no bob
        needle2 "small"]
    (with (p/run (try (tap (binding [hf/db       hf/*$*
                                     hf/*nav!*   nav!
                                     hf/entity   9
                                     hf/*schema* schema]
                             (Rows. (hfql {(orders needle1) [:order/email
                                                             {:order/gender [(props :db/ident {::hf/as gender})]}
                                                             {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                        ::hf/options (shirt-sizes gender needle2)})
                                                              [:db/ident]}]} ))))
                      (catch Pending _)))))
  % := [[0 ['(wip.orders-datascript/orders needle1)]]
        [1 [:order/email :order/gender :order/shirt-size]]
        [1
         ["alice@example.com"
          {:db/ident :order/female}
          [:select
           {:value {:db/ident :order/womens-large}}
           [:option {:db/ident :order/womens-small}]]]]
        [1
         ["charlie@example.com"
          {:db/ident :order/male}
          [:select
           {:value {:db/ident :order/mens-medium}}
           [:option {:db/ident :order/mens-small}]]]]])

(tests
  "free inputs"
  (with (p/run (try (tap (binding [hf/db       hf/*$*
                                   hf/*nav!*   nav!
                                   hf/*schema* schema]
                           (Rows. (hfql {(orders .) [{:order/gender [(props :db/ident {::hf/as gender})]}
                                                     {(props :order/shirt-size {::hf/render  Select-option-renderer
                                                                                ::hf/options (shirt-sizes gender .)})
                                                      [:db/ident]}]})) ) )
                    (catch Pending _))))
  % := [[0 ['(wip.orders-datascript/orders .)]]
        [1 [:order/gender :order/shirt-size]]
        [1 [{:db/ident :order/female}
            [:select
             {:value {:db/ident :order/womens-large}}
             [:option {:db/ident :order/womens-small}]
             [:option {:db/ident :order/womens-medium}]
             [:option {:db/ident :order/womens-large}]]]]
        [1 [{:db/ident :order/male}
            [:select
             {:value {:db/ident :order/mens-large}}
             [:option {:db/ident :order/mens-small}]
             [:option {:db/ident :order/mens-medium}]
             [:option {:db/ident :order/mens-large}]]]]
        [1 [{:db/ident :order/male}
            [:select
             {:value {:db/ident :order/mens-medium}}
             [:option {:db/ident :order/mens-small}]
             [:option {:db/ident :order/mens-medium}]
             [:option {:db/ident :order/mens-large}]]]]])

;;; Function navigation

(defn order-name [e]
  (first (str/split (:order/email (d/entity hf/*$* e)) #"@" 2)))

(s/fdef order-name :ret string?)

(tests
  "function navigation"
  (with (p/run (tap
                 (binding [hf/db     hf/*$*
                           hf/*nav!* nav!]
                   (Rows. (hfql [hf/*$* hf/db] {(orders "") [:db/id order-name]}) ) ))
          ))
  % := `[[0 [(wip.orders-datascript/orders "")]]
         [1 [:db/id hyperfiddle.hfql.explorer-tests/order-name]]
         [1 [9 "alice"]]
         [1 [10 "bob"]]
         [1 [11 "charlie"]]])


;;; Binding conveyance

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
                        (Rows. (hfql [*db* hf/db] {(bound-order "alice") [:db/id]})) ))
                    (catch Pending _))))
  % := `[[0 [(hyperfiddle.hfql.explorer-tests/bound-order "alice")]]
         [1 [:db/id 9]]])

