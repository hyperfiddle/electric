(ns dustin.y2022.hfql.hfql-interval-editor
  (:require #?(:clj [datomic.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.popover :refer [Popover]])
  (:import [hyperfiddle.photon Pending]
           [missionary Cancelled])
  #?(:cljs (:require-macros dustin.y2022.hfql.hfql-interval-editor)))


(p/defn Sub-request [absence-or-request]
  (hf/hfql
    {{{absence-or-request
       {:options (request-picklist search-short-code)
        :option-select (hf/replace-route! [`Sub-request absence-or-request])}}
      [:sub-req.index/short-code
       :sub-req/id]}

     [::request-kind


      {#render (popover (Interval-editor. absence-or-request)) {:label "interval editor"}}

      ; crate new as airtable style -
      ; slack, twitter, facebook -
      ; the new button is top or bottom, depending on the query and UX, such that the new thing appears
      ; "next" in the feed. how does HFQL do chatbox or newsfeed semantics?


      ; this is inline editing which we disallow. how to make safe?
      {{(request-intervals %) {:render ui/table
                               :table-new (hfql {(tempid!)
                                                 [:db/id
                                                  :interval/start
                                                  :interval/end]})}}
       [:db/id
        {:interval/start {:render (let [offset (p/server (nav id :interval/start-offset))]
                                    (-> interval/start (+ offset) (ui/datetime) (- offset)))}}
        {:interval/end {:render (let [offset (p/server (nav id :interval/end-offset))]
                                  (-> interval/end (+ offset) (ui/datetime) (- offset)))}}]}


      :sub-req/reason
      :sub-req/bucket
      :sub-req/approval
      :sub-req/who
      :sub-req/create-sub-req?]}))



(p/defn Interval-editor [absence-or-request]
  (hf/hfql
    {(request-intervals absence-or-request)
     [:db/id ; how to tempid?
      {:interval/start {:render (let [offset (p/server (nav id :interval/start-offset))]
                                  (-> interval/start (+ offset) (ui/datetime) (- offset)))}}
      {:interval/end {:render (let [offset (p/server (nav id :interval/end-offset))]
                                (-> interval/end (+ offset) (ui/datetime) (- offset)))}}]})

  #_(hf/hfql
      {{absence-or-request
        [request-kind
         :absence/id
         :sub-req/id]}
       [(request-intervals %)]}))

(comment
  (s/fdef interval-editor :args (s/cat :request-or-absence (s/and some? ref?)))

  (defmethod hf/tx (keyword `interval-editor) [ctx [e a v] props popover-ctx]
    #?(:cljs
       (let [[_ req-or-absence] @(:hypercrud.browser/route-defaults-symbolic popover-ctx)]
         (set! hyperfiddle.ui.popover/*signal :hyperfiddle.ui.popover/clear-stage) ; clear edits and rebuild them for the validated interface
         (interval-editor-batch-tx req-or-absence (hf/data popover-ctx)))))
  )

(defn request-intervals [e] ...) ; merged weird thing

(defn request-kind [e]
  (let [!e (d/entity hf/*$* e)]
    (cond
      (and (:absence/id !e) (:sub-req/_absence !e))
      :swinged.rosie/absence-with-request

      (and (:sub-req/id !e) (:sub-req/absence !e))
      (do
        #_(assert false "use the absence when available, not the request")
        ; this happens in re-entrant case, don't assert
        :swinged.rosie/request-with-absence)

      (:sub-req/id !e)
      :swinged.rosie/naked-request

      (:absence/id !e)
      :swinged.rosie/naked-absence

      :else
      :swinged.rosie/request-kind-unknown)))

