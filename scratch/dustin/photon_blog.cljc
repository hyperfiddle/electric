(ns dustin.photon-blog
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode vars local2 node thread]]
            [hyperfiddle.photon :as photon]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.api :as hf]
            [missionary.core :as m]
            [geoffrey.fiddle-effects
             :refer [submissions genders shirt-sizes submission-details
                     submission gender shirt-size]]
            [hyperfiddle.q2 :refer [hf-nav hfql exports]]
            [hfdl.impl.util :as u])
  )

(defnode typeahead-select [& {:keys []}])

(defnode example-form [db e]
  (dom/form
    (dom/field
      ::hf/label "shirt size"
      (typeahead-select
        ::hf/eav [e :dustingetz/shirt-size]
        ::options (node [needle]
                    (let [gender (thread (:dustingetz/gender (datomic.api/entity db e)))
                          es (thread
                               (datomic.api/q
                                 '[:in $ ?gender :find [?e ...] :where
                                   [?e :dustingetz/type :dustingetz/shirt-size]
                                   [?e :dustingetz/gender ?gender]]
                                 db gender (or needle "")))]
                      (for [e es]
                        (dom/option ~@(thread (:db/ident (datomic.api/entity db e)))))))))))

(defnode foo [db]
  (for [x]
    ~@(doto (goog.dom/createElement "tr")
        (goog.dom/appendChild
          (pr-str x)))))


(defnode page [el]
  (reagent.dom/render [:pre ~@(pr-str (datomic.api/q ...))] el))

(photon/main
  (page (js/document.getElementById ...)))



(defnode foo []
  (reagent.dom/render [:table (p/for [x (datomic.api/q ...)]
                                     [:tr (pr-str x)])]))

[:div [:pre ...]]



(reactRender)

(ReactDOM/render [:table (rfor [% :db/id @(datomic.api/q ...)] @(render-row ~%))])

(require)



(defn todos [status] (datomic.api/q '[:find [?e ...] :in ...] db status))
(s/fdef todos :args (s/cat :status keyword?) :ret sequential?)

(defnode todo-list [status]
  (with-redefs [for photon/for]
    (hfql
      {((todos status))
       [:db/id
        :task/name
        :task/status]})))


(def !status (atom :task/completed))
(photon/main (todo-list ~(m/watch !status)))
(reset! !status nil)

(tests
  ~(photon/main (todo-list :task/completed)) :=
  )

{((todos .) ::hf/render todo-table)
 [(:db/id ::hf/a (todo-detail %))
  :task/name
  :task/completed]}

(defnode render-submissions [xs props]
  (dom/div
    (dom/h1 "submissions")
    (dom/table
      (photon/for [{:keys [:db/id
                           :person/email
                           :person/gender
                           :person/shirt-size]} xs]
        (dom/tr
          (dom/td (dom/span (pr-str id)))
          (dom/td email)
          (dom/td (dom/span (pr-str gender)))
          (dom/td (dom/span (pr-str shirt-size))))))))

(s/fdef submissions :args (s/cat :needle string?) :ret sequential?)
(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?) :ret sequential?)

(hfql
  [{(submissions .)
    [:db/id
     :person/email
     {(:person/gender ::hf/options (genders))
      [:db/ident]}
     {(:person/shirt-size ::hf/options (shirt-sizes gender .))
      [:db/ident]}]}])

; single expression

(dom/div (pr-str (datomic.api/q ...)))

(reagent.dom/render [:pre (pr-str)])

; closure and lambda

(defnode)



(binding [dom/*parent* el]
  (dom/table
    (for [x (database/query ...)]
      (dom/tr
        (pr-str x)))))





(defnode typeahead-select [v options]
  (dom/div
    (let [needle (dom/input "")]
      (dom/select
        (for [x (options needle)]
          (dom/option (pr-str (database/query ... x))))))))

(typeahead-select v (node [needle] (database/query ... needle)))







(dom/form
  (dom/field
    (dom/label "shirt-sizes")
    (dom/select)
    )
  )

(render)



; problems
; 1. it is not reactive
; 2. it is a client/server system
; 3.

type








(defnode page [db]
  (js/console.log (str "the type is : " ~@(type "hello world"))))





