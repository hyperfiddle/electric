(ns dustin.rosie.subs2
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
  (dom/fragment
    (dom/h1 "Block sub from school")
    (dom/p "School has requested that the given sub be blocked from their school.
             This will always unlink the sub.")))

(def sub-display
  (hf/router
    {(sub-display {?sub [suber-name]})
     [(suber-name ?sub)
      :sub/id
      :db/id
      '(sub-requests ?sub)
      ('(school-requested-block
          {?sub [:sub/id]}
          {(?school
             ::hf/render picklist
             ::hf/options (school-picklist ?needle)
             ::hf/option-label :school/name)
           [:school/id :school/name]}
          {(?block-reason
             ::hf/render picklist
             ::hf/options (event-reason-codes-picklist ?needle)
             ::hf/option-label (comp name :db/ident))
           [:db/ident]}
          (?block-mode
            ::hf/render checkbox-picker
            ::hf/default (or ?block-mode :force-cancel)
            ::hf/options (unlink-mode-options)
            ::hf/option-label (let [wide {:style {:display "inline-block" :width "12em"}}]
                                {:force-cancel      (dom/span (dom/span wide ":force-cancel") "Cancel all the sub's existing commitment at this school")
                                 :leave-commitments (dom/span (dom/span wide ":leave-commitments") "Don’t cancel sub's existing commitments")}))
          (?penalize
            ::hf/default (if (some? ?penalize) ?penalize false)))
        ::hf/prompt render-school-requested-block
        ::hf/tx [(suber2.web.cmd/school-requested-block' ?school ?sub ?block-mode ?block-reason ?penalize)])
      ((sub-tags ?sub) ::hf/options (all-tags (?needle ::hf/default (or ?needle ""))))
      ((sub-locations ?sub) ::hf/options (all-locations (?needle ::hf/default (or ?needle ""))))
      {(:school/_subs
         ::hf/render ...
         ::hf/options (sub-schools-picker-options ?needle))
       [:school/id :school/name]}]}))
