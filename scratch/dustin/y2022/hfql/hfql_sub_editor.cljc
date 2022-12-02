(ns dustin.y2022.hfql.hfql-sub-editor
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  #?(:cljs (:require-macros dustin.y2022.hfql.hfql-sub-editor)))


(defn school-requested-block
  "School has requested that the given sub be blocked from their school.
  This will always unlink the sub."
  [sub school block-reason mode penalize] ...)

(s/fdef school-requested-block
        :args (s/cat
                :sub (s/and some? ref?)
                :school (s/and some? ref?)
                :block-reason (s/and some? ref?)
                :block-mode (s/and some? keyword?)
                :penalize boolean?))

(p/defn SubEditor [sub] ; route cycle
  (hf/hfql
    {{{sub {:options (sub-picklist search)
            :on-select (hf/replace-route! [:swinged.rosie/sub sub])
            :option-render suber-name}}
      [{suber-name {:link [:swinged.rosie/sub sub]}}]}

     [{suber-name {:tx (sub-rename e .)
                   :title "Sub rename"
                   :doc (str "Current name: " (dom/strong suber-name))}}
      {suber-email {:tx (change-email sub .)}} ; emit inline

      {(ground ; won't work, no specs for 'school etc
         {{school {:options (school-picklist .)
                   :option-render (str school/name " " (dom/span "(" school/district ")"))}}
          [:school/name :school/district]}

         {{block-reason {:options (block-reason-picklist .)
                         :option-render (name (:db/ident %))}}
          [:db/ident]}

         {block-mode {:default :force-cancel
                      :render (checkbox-picker %)
                      :options (unlink-mode-options)
                      :option-render (let [wide {:style {:display "inline-block" :width "12em"}}]
                                       (case block-mode
                                         :force-cancel (str (dom/span wide ":force-cancel") "Cancel all the sub's existing commitment at this school")
                                         :leave-commitments (str (dom/span wide ":leave-commitments") "Don’t cancel sub's existing commitments")))}}

         {penalize? {:default false}})
       {:tx (school-requested-block sub school block-reason block-mode penalize?)
        :popover true
        :popover-label `school-requested-block
        :title "Block sub from school"
        :doc (doc school-requested-block)}}

      {(change-sub-status
         {sub {}}
         {action {}})}

      :sub/phone
      notify-method
      :sub/tags
      {sub-locations {:options (sub-locations sub)}}
      {sub-metrics [:sub.metrics/feedback-score
                    :sub.metrics/reliability-score]}
      {{:security/role {:options (security-roles)}}
       [:db/ident]}]}))



(defn sub-rename [e name'] [[:db/add e :sub/name name']])
(s/fdef sub-rename :args (s/cat :name string?))
