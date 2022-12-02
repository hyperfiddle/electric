(ns dustin.y2022.hfql.hfql-sub-editor3
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.popover :refer [popover Popover]]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  #?(:cljs (:require-macros dustin.y2022.hfql.hfql-sub-editor3)))


(defn school-requested-block "School has requested to block sub from their school." [sub school] ...)
(s/fdef school-requested-block :args (s/cat :sub (s/and some? ref?) :school (s/and some? ref?)))

; everything is readonly by default
; opt in to writes by specfying HOW the tx should be formed
; queries are not dangerous - inline to extent possible without branching
; commands are dangerous - need confirm button, and a query to see it worked!


#_(row (cell (str `suber-name)) (cell (p/server (nav sub suber-name))))
#_(row (cell (str `sub-status)) (cell (p/server (nav sub sub-status))))

(p/defn CommandPopoverBody [sub]
  (hf/hfql
    #command (school-requested-block
               {{sub {:options (sub-picklist search)}}
                [suber-name]}
               {{school {:options (school-picklist .)}}
                [:school/name]})))

(p/defn CommandPopoverBody [sub]
  (row (cell (str `school-requested-block)))
  (loop [[sub school] [sub nil]]
    (let [sub (row (cell (str 'sub))
                   (cell (ui/typeahead {:value (p/server (hfql {sub
                                                                [suber-name]}))
                                        :options (p/fn [search]
                                                   (p/server (hfql {(sub-picklist search)
                                                                    [suber-name]})))})))

          school (row (cell (str 'school))
                      (cell (ui/typeahead {:value (p/server (hfql {school
                                                                   [:school/name]}))
                                           :options (p/fn [search]
                                                      (p/server (hfql {(school-picklist search)
                                                                       [:school/name]})))})))]
      (recur [sub school]))
    (when (valid? `(school-requested-block ~sub ~school))
      (school-requested-block sub school)))) ; emit inline tx - without button ???

(p/defn SubsPage [sub]
  (hf/hfql
    {(ground {{sub {:options (sub-picklist search)}}
              [suber-name]})
     [#command (school-requested-block ; inline !
                 sub
                 {{school {:options (school-picklist .)}}
                  [:school/name]})]}))

(p/defn SubsPage [sub]
  (hf/hfql
    {{{sub {:options (sub-picklist search)}}
      [suber-name]}
     [{#command (school-requested-block
                  sub
                  {{school {:options (school-picklist .)}}
                   [:school/name]})
       {:branch true}}]}))

; commands should get a popover (and be self contained and routable) for UX + the execute button + verify opportunity
; edits that act as inline command - are there such thing? should they all get commands?
; edits that act as inline command can have the popover suppressed

(p/defn SubsPage [sub]
  (hf/hfql
    {{{sub {:options (sub-picklist search)}}
      [suber-name]}
     [{nil {:label (str `school-requested-block)
            :render (popover {:anchor (ui/button "...")}
                      (hf/hfql
                        #command (school-requested-block
                                   {sub [suber-name]}
                                   {{school {:options (school-picklist .)}} [:school/name]})))}}]}))

(p/defn Block-sub-from-school-body [sub]
  (hf/hfql
    {sub {:title "Block sub from school"
          :doc (dom/dl
                 (dom/dt "Sub: ") (dom/dd sub)
                 (dom/dt "Status: ") (dom/dd (p/server (sub-status sub))))
          :options (sub-picklist search)
          :option-render suber-name
          :on-select (hf/replace-route! [:swinged.rosie/sub sub])}

     [#command (school-requested-block ; inline
                 sub
                 {{school {:options (school-picklist .)
                           :option-render (str school/name " " (dom/span "(" school/district ")"))}}
                  [:school/name :school/district]}
                 block-reason block-mode penalize?)]}))

(hf/hfql
  {#command (school-requested-block ; inline
              {{sub {:options (sub-picklist search)
                     :option-render suber-name}}
               [suber-name]}
              {{school {:options (school-picklist .)
                        :option-render (str school/name " " (dom/span "(" school/district ")"))}}
               [:school/name :school/district]}
              block-reason block-mode penalize?)
   {:title "Block sub from school"
    :doc (dom/dl
           (dom/dt "suber-name") (dom/dd (p/server (suber-name sub)))
           (dom/dt "sub-status") (dom/dd (p/server (sub-status sub))))}})

(p/defn Block-sub-from-school-body [sub]
  (hf/hfql
    {#command (school-requested-block ; inline
                {{sub {:options (sub-picklist search)
                       :option-render suber-name}}
                 [suber-name]}
                {{school {:options (school-picklist .)
                          :option-render (str school/name " " (dom/span "(" school/district ")"))}}
                 [:school/name :school/district]}
                block-reason block-mode penalize?)
     #_[] ; commands don't have continuations
     {:title "Block sub from school"
      :doc (dom/dl
             (dom/dt "suber-name") (dom/dd (p/server (suber-name sub)))
             (dom/dt "sub-status") (dom/dd (p/server (sub-status sub))))}

     (ground sub) {:title "Block sub from school"
                   :doc (dom/dl
                          (dom/dt "Sub: ") (dom/dd sub)
                          (dom/dt "Status: ") (dom/dd (p/server (sub-status sub))))
                   :options (sub-picklist search)
                   :option-render suber-name
                   :on-select (hf/replace-route! [:swinged.rosie/sub sub])}

     []}))

(p/defn Block-sub-from-school-body [sub]
  (hf/hfql
    {(ground sub) {:title "Block sub from school"
                   :options (sub-picklist search)
                   :option-render suber-name
                   :on-select (hf/replace-route! [:swinged.rosie/sub sub])}}

    #command (school-requested-block
               sub
               {{school {:options (school-picklist .)
                         :option-render (str school/name " " (dom/span "(" school/district ")"))}}
                [:school/name :school/district]}
               block-reason block-mode penalize?)))


(p/defn SubEditor [sub] ; route cycle
  (hf/hfql
    {{{(ground sub) {:options (sub-picklist search)
                     :on-select (hf/replace-route! [:swinged.rosie/sub sub])
                     :option-render suber-name}}
      [{suber-name {:link [:swinged.rosie/sub sub]}}]}

     [{suber-name {:title "Sub rename"
                   :doc (str "Current name: " (dom/strong suber-name))
                   :tx (sub-rename sub suber-name)}} ; prompt for new name, is this a button "rename" next to the form to prompt for . ?
      {suber-email {:tx (change-email sub .)}} ; emit inline

      {(suber-email % fallback) ; queries are not dangerous, inline ui
       []}

      {sub-status
       {}}


      ; is a fork even needed? This could be inline?
      ; What about the discrete nature of the command?
      ; commands happen once and need confirmation
      ; it could be inline, but it's dangerous
      {#command (school-requested-block sub school block-reason block-mode penalize?)
       {:title "Block sub from school"}}

      ; queries have continuations
      ; commands do not

      ; draw the inline version first

      {

       ; to verify the result before commit
       {:title "Block sub from school"
        :tx (school-requested-block
              sub
              {{school {:options (school-picklist .)
                        :option-render (str school/name " " (dom/span "(" school/district ")"))}}
               [:school/name :school/district]}
              block-reason block-mode penalize?)}}



      ; command works on any sub, make commands self contained and routable
      {{sub {:options (sub-picklist search)
             :option-render suber-name}
        [sub-status]} ; to verify the result before commit
       {:title "Block sub from school"
        :tx (school-requested-block
              sub
              {{school {:options (school-picklist .)
                        :option-render (str school/name " " (dom/span "(" school/district ")"))}}
               [:school/name :school/district]}
              block-reason block-mode penalize?)}}

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
