(ns dustin.rosie.subs1
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as d]
            [hyperfiddle.api :as hf :refer [hfql]]
            [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :as rcf :refer [tests tap %]]
            [missionary.core :as m]
            ))

(p/defn suber-name [e]
  )

(p/defn sub-tags [e]
  #?(:clj (->> (:sub/tags (d/entity hf/*$* e))
               (remove #(re-find filter-out-tags-regexp (or (namespace %) "")))
               vec)))

(p/defn sub-locations [e]
  #?(:clj (->> (:sub/tags (d/entity hf/*$* e))
               (filter #(= (namespace %) "location"))
               vec)))

(p/defn sub-quals [e]
  #?(:clj (doall
            (->> (:sub/attrs (d/entity hf/*$* e))
                 (mapv #(into {} %))))))

(p/defn sub-metrics-kv [sub]
  ; note return type is map
  #?(:clj (d/q '[:find (pull ?e [:sub.metrics/feedback-score
                                 :sub.metrics/reliability-score]) .
                 :in $ ?sub
                 :where [?e :sub.metrics/sub ?sub]]
               hf/*$* sub)))

(defn sub-onboarding-date [e]
  #?(:clj (suber2.web.cmd/onboard-date hf/*$* (d/entity hf/*$* e))))

(defn sub-approx-days-worked [e]
  ;; Days-claimed is accurate enough to avoid the invoice pipeline
  #?(:clj (-> (d/entity hf/*$* e)
              :sub-stats/_sub
              first
              :sub-stats/days-worked)))

(defn sub-status [e]
  #?(:clj (->> (d/entity hf/*$* e)
               (:sub/tags)
               (filter #{:status/suspended :sub/deactivated :status/removed})
               (first))))

(s/fdef all-tags :args (s/cat :needle string?))
(p/defn all-tags [needle]
  #?(:clj
     (->> (d/q '[:find [?tag ...]
                 :in $ ?needle
                 :where
                 [_ :sub/tags ?tag]
                 [(swinged.rosie.subs/ns-different "geocode" ?tag)]
                 [(swinged.rosie.subs/ns-different "location" ?tag)]
                 [(rosie.rosie-common-util/needle-match ?tag ?needle)]]
               hf/*$* (str/trim (or needle "")))
          (sort-by (juxt namespace name))
          (vec))))

; render

(p/defn render-sub-display [sub]
  (dom/h1 "Sub: " (server (suber-name ~sub))))

(p/defn render-sub-email [v]
  (p/binding [hf/view-change! (p/fn [old new]
                                (runtime/with-tx ctx [`(change-email ~(hf/e ctx) ~new)]))]
    (hf/string v)))

;(defmethod hf/tx-meta `change-email
;  [_schema [f e & _]]
;  {::hf/tx-identifier   e
;   ::hf/tx-cardinality  ::hf/one
;   ;; ::hf/tx-inverse      `(change-email ~e ~new ~old)
;   ::hf/tx-conflicting? (fn [[f' e' & _]] (and (= f f') (= e e')))})

;; Warning:
;; - `suber.web.cmd/display* :sub` only shows `:google/email`.
;; - in dev, `:google/email` is excised, if we edit it, it will incorrectly fill
;; the `:cognito/email` field. Dev-only issue. See `suber2.web.cmd/change-email-tx`
(defn change-email
  "tx-data to change a given user's email address from WRONG-EMAIL to CORRECT-EMAIL."
  [sub correct-email]
  #?(:clj
     (let [{:keys [db/id] :as u-entity} (d/entity hf/*$* sub)]
       (binding [suber.web.globals/*db* hf/*$*]
         (suber2.web.cmd/change-email-tx id u-entity correct-email)))))

(s/fdef all-locations :args (s/cat :sub any? :needle string?))
(p/defn all-locations [needle]
  #?(:clj (into [] (comp (take hf/browser-query-limit)
                         (map #(nth % 2)))
                (rosie.rosie-sub/list-locations2 needle))))

(p/defn sub-display [e] e)
(s/fdef sub-display :args (s/cat ::sub (s/and some? ref?)) :ret (s/keys))

(s/fdef school-picklist :args (s/cat :_ any? :needle string?))
(p/defn school-picklist [needle]
  #?(:clj
     (into []
           (take hf/browser-query-limit)
           (d/q '[:in $ ?needle
                  :find [(pull ?e [:school/name
                                   :school/id]) ...]
                  :where
                  [?e :school/id]
                  [?e :school/name ?name]
                  [(rosie.rosie-common-util/needle-match ?name ?needle)]]
                hf/*$* needle))))

(s/fdef event-reason-codes-picklist :args (s/cat :needle string?))
(p/defn event-reason-codes-picklist [needle]
  #?(:clj (doall (take hf/browser-query-limit (event-codes needle)))))

(p/defn unlink-mode-options []
  [:force-cancel
   :leave-commitments])


(s/fdef school-requested-block :args (s/cat
                                       :sub (s/and some? ref?)
                                       ::school (s/and some? ref?)
                                       ::block-reason (s/and some? ref?)
                                       ::block-mode (s/and some? keyword?)
                                       ::penalize? boolean?))
(p/defn school-requested-block [sub school block-reason mode penalize] nil)

(p/defn render-school-requested-block [sub school block-reason mode penalize]
  ; #?(:cljs [`(suber2.web.cmd/school-requested-block' ~school ~sub ~mode ~block-reason ~penalize)])
  (dom/fragment
    (dom/h1 "Block sub from school")
    (dom/p "School has requested that the given sub be blocked from their school.
      This will always unlink the sub.")
    ; from-spec enriched
    (dom/form
      (dom/field :sub (hf/ref (d/pull hf/*$* [:sub/id] sub)))
      (dom/field ::school (hf/ref
                            ::hf/options (school-picklist ?needle)
                            (d/pull hf/*$* [:school/id :school/name] school)))
      )
    [

     (d/pull hf/*$* [:db/ident] block-reason)
     mode
     penalize]

    ))



(def sub-display
  (hf/router
    {((sub-display ?sub) ::hf/prompt render-sub-display)
     [(:db/id ::hf/a (sub-blocked-list ?sub))
      (:db/id ::hf/a (sub-feedback ?sub))
      (:db/id ::hf/a (sub-requests ?sub))
      (:db/id ::hf/a (rosie-entity-history ?sub))
      ;[:hf/iframe `sub-rename :tx-fn ":zero"]
      (:db/id ::hf/modal (school-requested-block ?sub ?school ?block-reason ?mode ?penalize))
      ;[:hf/iframe `district-requested-block :tx-fn (str `district-requested-block)]
      ;[:hf/iframe `alias-to :tx-fn (str `alias-to)]
      ;[:hf/iframe `change-sub-status :tx-fn (str `change-sub-status)]
      ;[:hf/iframe `sub-schools-picker-options]
      ;:sub-attr/keyword [[`qual-history :formula ::qual-history]]
      :sub/id
      #_:google/display-name #_:sub/name #_#_:sub/given-name :sub/family-name ; Norby uses get-name function
      (:google/email ::hf/render render-sub-email) #_:cognito/email #_:google.index/normalized-email
      :sub/phone #_:sub.index/normalized-phone              ;  for conflict detection
      :sub/phone-confirmed
      :sub/about
      :sub/photo
      {:esub/region [:region/id]}
      #_:geocode/data
      {:sub/priority [#_:sub-priority/level :sub-priority/id #_:string/label]}
      {:sub/experience ['*]}
      #_{:utm/pairs [:utm/key :utm/value]}
      :sub.index/cancel-rate #_:sub.index/cancel-rate-decile
      {:school/_subs [:school/id :school/name]}
      #_:sub/typeform
      #_{:pending-sub/onboard-attrs [*]}

      ((suber-name ?sub) :as ::rosie/name)
      ((sub-tags ?sub) :as :sub/tags ::hf/options (all-tags (or ?needle ""))) ;::hf/needle-key :needle :html/id `sub-tags-picker

      ((sub-locations ?sub) :as ::locations ::hf/options (all-locations (or ?needle "")))
      ((sub-quals ?sub) :as ::quals)
      ;((sub-metrics-kv ?sub) #_:as)
      ((sub-onboarding-date ?sub) :as ::onboard-date)
      ((sub-approx-days-worked ?sub) :as :sub-stats/days-worked)
      ((sub-status ?sub) :as ::sub-status)]

     ((school-requested-block
        (?sub
          ::hf/enrich (d/pull hf/*$* [:sub/id] ?sub))
        (?school
          ::hf/options (school-picklist ?needle)
          ::hf/option-label :school/name
          ;::hf/needle-key   :needle
          ::hf/enrich (d/pull hf/*$* [:school/id :school/name] ?school))
        (?block-reason
          ::hf/options (event-reason-codes-picklist ?needle)
          ::hf/option-label (comp name :db/ident)
          ;::hf/needle-key   :needle
          ::hf/enrich (d/pull hf/*$* [:db/ident] ?block-reason))
        (?mode
          ::hf/default (or ?mode :force-cancel)
          ::hf/options (unlink-mode-options))
        (?penalize
          ::hf/default (if (some? ?penalize) ?penalize false)))
      ::hf/prompt render-school-requested-block
      ::hf/tx [(suber2.web.cmd/school-requested-block' ?school ?sub ?mode ?block-reason ?penalize)])
     []}))

(s/def ::quals (s/coll-of (s/keys)))
(s/def ::locations (s/coll-of keyword?))
(s/def ::feedback-score float?)
(s/def ::reliability-score float?)
(s/def ::onboard-date inst?)

(def sub-blocked-list
  (hf/router
    {(sub-blocked-list)
     [:db/id]}))

(def sub-blocked-list
  (hf/router
    {(sub-feedback)
     [:db/id]}))

(def sub-blocked-list
  (hf/router
    {(sub-requests)
     [:db/id]}))
