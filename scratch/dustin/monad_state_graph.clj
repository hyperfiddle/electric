(ns dustin.monad-state-graph
  (:require
    [contrib.do :refer [! via* *this Do-via]]
    [datomic.api :as d]
    [hyperfiddle.api :as hf]
    [minitest :refer [tests]]
    [user.scratch])
  (:import
    (datomic.db Db)))


(extend-type Db
  Do-via
  (resolver-for [_]
    {:Graph.entity (fn [[_ e]] (d/entity *this e))
     :Graph.pull (fn [[_ pat e]] (d/pull *this pat e))
     :Graph.with   (fn [[_ tx]]
                     (set! *this (:db-after (d/with *this tx))))}))

(defn flip-gender "update the gender, returning the entity"
  [e]
  (let [s (:dustingetz/gender (! :Graph.entity e))
        s' (case s
             :dustingetz/male :dustingetz/female
             :dustingetz/female :dustingetz/male)]
    (! :Graph.with [[:db/add e :dustingetz/gender s']])
    e))

(tests
  (def alice [:dustingetz/email "alice@example.com"])

  (via* (hf/get-db "$")
    (-> (! :Graph.entity alice)
      :dustingetz/email))
  := "alice@example.com"

  (via* (hf/get-db "$")
    (flip-gender alice))
  := [:dustingetz/email "alice@example.com"]

  (via* (hf/get-db "$")
    (->> alice
      flip-gender                                           ; flip gender
      flip-gender                                           ; flip back
      (! :Graph.pull [:db/id :dustingetz/email
                      {:dustingetz/gender [:db/ident]}
                      {:dustingetz/shirt-size [:db/ident]}])))
  := {:db/id                 17592186045428,
      :dustingetz/email      "alice@example.com",
      :dustingetz/gender     {:db/ident :dustingetz/female},
      :dustingetz/shirt-size {:db/ident :dustingetz/womens-large}}
  )

(defrecord Entity [id]
  Do-via
  (resolver-for [_]
    {:Entity.pull (fn [[_ pat]]
                    (! :Graph.pull pat id))}))

(defn html-prop-adapter [props]
  (-> props
    (update :html/class #(clojure.string/join " " %))
    (update :html/id name)
    (clojure.set/rename-keys {:html/class :class
                              :html/id :id})))

(defn field [props]
  [:input (html-prop-adapter props)])

(tests
  (via* (hf/get-db "$")
    (! :Graph.with [{:db/ident :html/class :db/cardinality :db.cardinality/many :db/valueType :db.type/string}
                    {:db/ident :html/id :db/cardinality :db.cardinality/one :db/valueType :db.type/keyword :db/unique :db.unique/identity}])

    (! :Graph.with [{:html/id ::foo :html/class ["b"]}])
    (! :Graph.with [[:db/add [:html/id ::foo] :html/class "a"]])

    (via* (->Entity [:html/id ::foo])
      (field (! :Entity.pull [:html/class
                              :html/id]))))

  := [:input {:class "a b", :id "foo"}]
  )
