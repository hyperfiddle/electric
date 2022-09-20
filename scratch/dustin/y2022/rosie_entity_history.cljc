(ns dustin.y2022.rosie-entity-history
  (:require
    #?(:clj [backtick :refer [template]])
    [clojure.spec.alpha :as s]
    #?(:clj [clojure.core.match :refer [match]])
    [contrib.data :refer [unqualify]]
    #?(:clj [datomic.api :as d])
    [hyperfiddle.api :as hf]
    [hyperfiddle.spec.datomic :refer [ref?]]
    #?(:cljs [hyperfiddle.ui :as ui])
    #?(:cljs [hyperfiddle.ui.select :refer [picklist]])
    [swinged.rosie :as rosie]
    #?(:clj [swinged.norby2 :refer [suber-name-kv]])
    #?(:clj suber2.web.cmd.names)))


(defn get-norby-name [$ e]
  #?(:clj
     (if (= e ::no-name)
       "n/a"
       (suber2.web.cmd.names/get-name 'unknown (d/entity $ e)))))

(defn infer-type [$ e]
  #?(:clj
     ; coerce to entity? e is a scalar
     (match (d/entity $ e)
            {:sub/id _} ::sub
            {:admin/id _} ::admin
            _ nil)))

(defn get-norby-entity-name [$ a v]                         ; v is a scalar
  #?(:clj
     (if (not= :db.type/ref (:db/valueType (d/entity $ a)))
       (str v)
       (let [t (infer-type $ v)]
         (match t
                ::sub (get-norby-name $ v)
                ::admin (get-norby-name $ v)

                ; Not sure what to fallback to
                _ (str v))))))

(comment
  (d/touch (d/entity $ 17592219419387))
  (d/touch (d/entity $ 17592189957033))
  (select-keys (d/entity $ 17592219419387) [:sub/id :admin/id])
  (infer-type $ 17592219419387)
  (infer-type $ 17592189957033)
  #_(infer-type $ (d/entity $ 17592189957033))
  (get-norby-entity-name $ 17592189957033)
  )

(defn tx-memo "Try to render something sensible, the schema here is pretty loose as this seems
  mostly about diagnostics and not used in the application. Collecting :event/type is preferred strategy"
  [$ tx]
  #?(:clj
     (or
       (:event/type (d/entity $ tx))
       (:event/memo (d/entity $ tx))
       (some->> (d/entity $ tx)
                :event/children
                (map :event/type) ; (juxt :event/type :event/memo)
                (seq)
                (into #{})))))

(comment
  (tx-memo hf/*$* 13194178359186)
  (tx-memo hf/*$* 13194177564889)
  (tx-memo hf/*$* 13194177562430)
  (d/touch (d/entity hf/*$* 13194177562430))
  (d/touch (d/entity hf/*$* 13194178359186))

  (->> [13194178359186
        13194177564912
        13194177564894
        13194177564910
        13194177757494
        13194177564896
        13194177564899
        13194177564909
        13194177756978
        13194177892049
        13194177756978
        13194177757494
        13194177564903
        13194177564900
        13194177757494
        13194177770099
        13194177564897
        13194177564885
        13194177564889
        13194177756978
        13194177564902
        13194177564894
        13194177756909
        13194177562430
        13194177564889]
       (map (fn [e]
              (tx-memo hf/*$* e)))
       #_(mapcat (fn [e]
                   (->> (d/entity hf/*$* e) :event/children)))
       #_(map (fn [{e :db/id}]
                (d/touch
                  (d/entity hf/*$* e)))))
  )

(s/def ::a keyword?)
(s/def ::v string?)
(s/def ::e string?)
(s/def ::op keyword?)
(s/def ::issued-by keyword?)
(s/def ::who string?)
(s/def ::memo string?)                                      ; hack

#?(:clj (defn before? [^java.util.Date a ^java.util.Date b] (<= (.getTime a) (.getTime b))))


(defn- xf-map-datoms [f]
  #?(:clj
     (map (fn [[e a v tx added?]]
            (let [tx-entity (d/entity hf/*$* tx)]
              (f [(:db/txInstant tx-entity)
                  (:db/id (:event/who tx-entity ::no-name))
                  e
                  (:db/ident (d/entity hf/*$* a))
                  v
                  (swinged.rosie.history/tx-memo hf/*$* tx)
                  added?
                  (:event/issuing-service tx-entity)]))))))

(defn- xf-filter-before-date [before] #?(:clj (filter (fn [[t _who _a _v _memo _added? _service]] (before? t before)))))

(defn- sort-datoms-by-time
  [[time  who  e  a  v  memo  added  issuing-service]
   [time' who' e' a' v' memo' added' issuing-service']]
  ; Draw :add as more recent than :retract for the same attribute
  (compare [time' a  added']
           [time  a' added ]))

(defn reverse-keyword
  " (reverse-keyword :foo/bar) := :foo/_bar "
  [kw]
  (keyword (namespace kw) (str "_" (name kw))))

(defn ^{::hf/fiddle {:shape '[:find [(pull ?e [*]) ...] :in $ ?e :where [?e]]
                     :links {`rosie-entity-history [[:hf/iframe `datomic-attrs-picklist]
                                                    [:hf/iframe `nothing]]}}}
  rosie-entity-history "see :norby.fn/show-history"
  [e before attr]
  (assert e)
  #?(:clj
     (let [history (d/history hf/*$*)]
       (into [] (comp
                  (take hf/browser-query-limit)                      ; route param
                  (map
                    (fn [[time who-id e a v memo added service]] ; presentation
                      ; (d/pull $ [a] e) ; if a is :db.cardinality/many, this explodes
                      ; hyperfiddle.ui can't render this properly due to dependent types
                      ; Coerce everything to strings, for now.
                      {:db/txInstant time
                       ::who         (get-norby-name hf/*$* who-id)
                       ::e           (str e) ; possible lookup ref, so this is a complex spec or `any?
                       ::a           a    ; can't repurpose :db/ident due to :db.unique/identity
                       ; If ::v is a ref, need to query out a human readable identity
                       ::v           (get-norby-entity-name hf/*$* a v) ; this is of spec `any? , dependent on a. So coerce to string for now.
                       ::op          (unqualify (if added :db/add :db/retract))
                       ; event/type
                       ::memo        (pr-str memo)                   ; yolo - keyword or set of keywords right now
                       ::issued-by   service})))
             (sort sort-datoms-by-time
                   (concat (sequence (comp (xf-map-datoms identity) (xf-filter-before-date before))
                                     (d/datoms history :eavt e attr))
                           (sequence (comp (xf-map-datoms (fn [[t who e a v memo added? issuing-service]]
                                                            #_[t who v (reverse-keyword a) e memo added? issuing-service]
                                                            [t who e a v memo added? issuing-service]))
                                           (xf-filter-before-date before))
                                     (d/datoms history :vaet e attr))))))))

(s/fdef rosie-entity-history
        :args (s/cat :entity (s/and some? ref?) :before inst? :attr ref?)
        :ret (s/coll-of (s/keys :req [::e ::a ::who ::v ::op ::memo]))) ; list the datomic/spec bridge keywords

(defmethod hf/defaults `rosie-entity-history [e before attr]
  #?(:clj [e (or before (java.util.Date.)) attr]))

(defmethod hf/view-defaults `rosie-entity-history [e before attr]
  #?(:clj [(suber-name-kv e)
           before
           (d/pull hf/*$* [:db/ident] attr)]))

(defmethod hf/render #{:entity `rosie-entity-history} [ctx props]
  #?(:cljs
     [picklist ctx (merge props {:disabled         true
                                 :html/id          `rosie-history-attr-picker
                                 ::hf/options      `nothing
                                 ::hf/needle-key   :needle
                                 ::hf/option-label ::rosie/name})]))

(defmethod hf/render #{:attr `rosie-entity-history} [ctx props]
  #?(:cljs
     [picklist ctx (merge props {:html/id          `rosie-history-attr-picker
                                 ::hf/options      `datomic-attrs-picklist
                                 ::hf/needle-key   :needle
                                 ::hf/option-label :db/ident})]))

(defmethod hf/render #{:before `rosie-entity-history} [ctx props]
  #?(:cljs
     [:div                                                  ; hack https://github.com/hyperfiddle/hyperfiddle/issues/1103
      [hyperfiddle.ui.controls/date (hf/data ctx) ctx props]]))

(comment
  (rosie-entity-history [:sub/id #uuid "5d41b392-49fe-4474-8123-040ad8cd63d7"])
  (time (->> x (take 20))))

(defn ^{::hf/fiddle {:shape '[:find [(pull ?e [*]) ...] :in $ ?e :where [?e]]}}
  datomic-attrs-picklist [needle]
  #?(:clj
     (into []
           (comp
             (remove #(= (namespace (:db/ident %)) "db"))       ; remove :db/ident to cause infinite loop
             (take hf/browser-query-limit))
           (->>
             (d/q '[:in $ ?needle
                    :find [(pull ?e [:db/ident]) ...]
                    :where
                    [?e :db/ident ?ident]
                    [(rosie.rosie-common-util/needle-match ?ident ?needle)]]
                  hf/*$* needle)
             (sort-by :db/ident)))))

(s/fdef datomic-attrs-picklist :args (s/cat :needle string?))
(defmethod hf/defaults `datomic-attrs-picklist [needle]
  #?(:clj [(or needle "")]))

(defn ^{::hf/fiddle {}} nothing [needle])                   ; hack to show a picklist
(s/fdef nothing :args (s/cat :needle string?))
