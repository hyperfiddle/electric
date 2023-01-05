(ns dustin.y2022.hfql.hfql-sub-editor4
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  #?(:cljs (:require-macros dustin.y2022.hfql.hfql-sub-editor4)))


#?(:clj (defn school-requested-block "School has requested to block sub from their school." [sub school] ...))
(s/fdef school-requested-block :args (s/cat :sub (s/and some? ref?) :school (s/and some? ref?)))

#?(:clj (defn sub-picklist [search] ...))
#?(:clj (defn suber-name [e] ...))

#?(:clj (defn alias-me [target] [[:db/add target :user/alias hf/*user'*]]))
(s/fdef alias-me :args (s/cat :target (s/and some? ref?)))

; all commands branch, for inline edits like pay rate editor, use ::hf/inline true with a single command

; 1. sub can be a constant
;(hfql (orders .))
;(hfql orders) ; sugar
;(hfql {9 [suber-name (suber-name %)]})
;(let [sub] (hfql sub)) ;collision with orders, no problem IRL

; 2. all query parameters (gray) cycle to the route. (recursively - tree router)
; Q: Can you overload gray input if the value comes from the query context? [shirtsize gender case] NO
; hypothesis: all orange values are readonly, all gray values are editable, unless unified with orange value

; 3. value and :options share continuation

; 4. flattened props syntax, are there conflicts? not really
;
;(hfql {sub [:db/id]})
;(hfql {sub {:render (do)}})
;(hfql {{sub {:render (do)}}
;       [:db/id]})
;(hfql {sub [{:db/id {:render (do)}}]})
;
;(hfql {{(query-all-entities-index ; should this be evaluated by clojure (map literals) or compiled by hfql?
;          {school [:school/name]}
;          {school [:school/name]}
;          {. {:render (do)}}
;          {. {}}
;          {. {}})
;        {:render (do)}}
;       [:db/id]})
;
;(hfql {{(query-all-entities-index
;          {. {:render (do)}}
;          {. {}}
;          {. {}})
;        [:db/id]}
;       {:render (do)}}) ; cant put props on a nested pull?
;
;(hfql {{{sub {:options (sub-picklist search)
;              :option-select (hf/replace-route! [`SubsPage sub])}}
;        [suber-name]}
;       [:db/id]})
;
;(defn query-index [a b] ...)
;(s/fdef query-index ...)
;
;(p/defn fff [a b]
;  (hfql {{(query-index
;            {{a {:options (sub-picklist search)
;                 :option-select (hf/replace-route! [`SubsPage a b])}}
;             [suber-name]}
;            {{b {:options (sub-picklist search)
;                 :option-select (hf/replace-route! [`SubsPage a b])}}
;             [school-name]})
;          {:link [`SubsPage a]
;           :render (hf/link [`SubsPage a] suber-name)}}
;         [:db/id]}))


; 5. links/commands - are they gray or orange?
; Hypothesis: Gray is route state. Commands are dangerous, they are orange and go in the body.
; Hypothesis: Links are like commands, put them in the body too.

;; 6. Syntax for links
;(hfql {sub
;       [{:db/id {:link [`SubsPage sub]
;                 :render (hf/Link. [`SubsPage sub] id)}} ; same
;        (hf/Link. [`Sub-requests sub] id)
;
;        ; these are all the same
;        #link [`Sub-requests %]
;        (hf/Link. [`Sub-requests sub])
;        #link `Sub-requests ; is this canonical? Rosie just wants to list where you can go next
;        #link [`Sub-requests sub] ; `(SubsPage. ~sub)
;        {nil [#link [`Sub-requests sub]
;              {:link [`Sub-requests sub]}]}
;        {sub [#link [`Sub-requests sub]
;              {:link [`Sub-requests sub]}]}]})

; 7. Commands
; Hypothesis: command anchors are orange; if you block sub from school you'd like to see the state transition happened.
; Hypothesis: there are no inline-tx, even simple edits are dangerous and thus lifted to commands
; Commands are photon exprs (in case you need to query)

;8. rendering
; it's not a p/fn anymore, it's a p expression, weave expr with lexical scope

(p/defn School-requested-block [sub]
  (let [school (ui/select {::ui/options (p/fn [search] (p/server (school-picklist search)))
                           ::ui/option-label (p/fn [x] (p/server (hf/nav x :school/name)))})
        block-mode nil
        block-reason nil
        penalize? nil]
    (when (valid?)
      (school-requested-block sub school))))

;(hfql {(q alice) [:db/id]})
;(hfql {[[:db/add 1 2 3]] [:db/id]})

(p/defn SubsPage [sub]
  (hf/hfql
    {{{sub {:options (sub-picklist search)
            :option-select (hf/replace-route! [`SubsPage sub])}}
      [suber-name]} ; this is the picklist continuation, no commands or links here

     [{{:db/id {:link [`SubsPage sub]
                :render (do (dom/p "sadf") (hf/link [`SubsPage sub] id))}}
       [...
        ...]} ; same

      #link [`Sub-requests sub] ; sugar for {nil {:link [`Sub-requests sub]}}   ?
      #link [`Sub-schools sub]

      ; label is static; cell has dynamic value
      {"sub-requests" {:link [`SubsPage sub]}}
      {"sub-requests" #link [`Sub-schools sub]}
      {"sub-requests" (do (dom/p "sadf") (hf/link [`SubsPage sub] id))} ; forms/tables have label and cell
      {:db/id %}
      {:db/id []}


      {:user/memo {:command (edit-user-memo % .)
                   :command (Edit-user-memo. % .)
                   :command [[:db/add % :user/memo .]]
                   :command [`(change-user-memo % .)]
                   :inline true}} ; inlines a commit button as well

      {#command (school-requested-block
                  sub
                  {{school {:options (school-picklist .)}}
                   [:school/name]})
       {:title "Block sub from school"}} ; never a continuation (transactions don't have continuations), can have props
      ; label = "school-requested-block" cell = (button "...")


      Query-school-for-sub-reactively.
      query-school-for-sub-slow
      (Query-school-for-sub-reactively. sub) ; efficient incremental query
      (query-school-for-sub-slow sub) ; (Cell :label "(query-schools-for-sub-slow sub)" :value 123124)
      {Query-school-for-sub-reactively. [:db/id]}
      {query-school-for-sub-slow [:db/id]}
      {(Query-school-for-sub-reactively. sub) [:db/id]}
      {(Query-school-for-sub-reactively. {sub {:options (sub-picklist .)}}) [:db/id]}
      {(query-school-for-sub-slow sub) [:db/id]}

      {#photon-escape (School-requested-block. sub) ; (Cell :label "(School-requested-block. sub)" :value (render effects)}
       {:label "block sub from school"}}
      ; label = sexpr; cell = eval, no param customization possible

      {#command (alias-me sub) {:render ...
                                ::hf/popover-body
                                (p/client
                                  (if (p/server (= (-> (d/entity hf/*$* sub) :db/id)
                                                   (-> (d/entity hf/*$* hf/*user'*) :user/alias :db/id)))
                                    (dom/p "You are already aliased to " suber-name "."
                                      "You can remove this alias from your account page.")
                                    (dom/p "Alias me to" suber-name "."
                                      "You are authenticated as" (suber-name hf/*user*'))))}}
      {#photon-escape (Alias-me. sub)} ; label = "(Alias-me. sub)" cell = (button "...")

      ; commands get their own cell, and don't forget about tables
      {#command (sub-rename sub .) {:anchor-label (p/server (suber-name sub))}}
      {#command (change-email sub .) {:anchor-label (p/server (suber-email sub))}}

      ; one cell should do one thing.

      ; alternate syntax - in forms, N commands render in the same row. in tables there is an issue
      ; no room for multiple commands, especially in table cells
      #_{suber-name {:commands [(sub-rename sub .)]}}
      #_{suber-email {:commands [(change-email sub .)]}}]}))

(p/defn Alias-me [sub]
  (when-some [e (ui/popover
                  (p/client
                    (if (p/server (= (-> (d/entity hf/*$* sub) :db/id)
                                     (-> (d/entity hf/*$* hf/*user'*) :user/alias :db/id)))
                      (dom/p "You are already aliased to " suber-name "."
                        "You can remove this alias from your account page.")
                      (dom/p "Alias me to" suber-name "."
                        "You are authenticated as" (suber-name hf/*user*')))))]
    (alias-me sub)))