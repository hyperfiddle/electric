(ns create-admin-2
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]))

(defn tempid! [] "-1")

(defn create-admin-query [admin] admin)
(s/fdef create-admin-query :args (s/cat :admin ref?))

(p/defn AdminPage []
  (hf/hfql {(props (create-admin-query
                     (props admin {::hf/options (admin-picklist-by-email .)
                                   ::hf/new-option (fn [search] [{:db/id (tempid!) :admin/email search}])})
                     name
                     email)
                   {:popover true
                    :popover-anchor-label "create-admin"
                    :render ...
                    :tx (suber.web/create-admin' admin :admin name email)})
            [:db/id
             suber-name
             :security/role
             :security/restrictions]}))

(p/defn AdminPage []
  (hf/hfql [{(props (all-admins-by-email email) {:render grid
                                                 ::hf/tx (p/fn [e a v] [[:db/add e a v]])})
             [:db/id
              suber-name
              :google/email
              (deactivate-admin)]}

            {(props (tempid!) {:render grid
                              ::hf/tx (suber.web/create-admin' (tempid) :admin name email)})
             [:db/id
              suber-name
              :google/email
              #_(deactivate-admin)]}
            ])

  (hf/hfql {(props (create-admin-query
                     (props admin {::hf/options (admin-picklist-by-email-with-new .)})
                     name
                     email)
                   {:popover true
                    :popover-anchor-label "create-admin"
                    :render ...
                    :tx (suber.web/create-admin' admin :admin name email)})
            [:db/id
             suber-name
             :security/role
             :security/restrictions]}))

; Assumption:
; we've mapped out the examples and agree that everything is coherent

; 1. enumerate the aspects - query, grid edits, inline tx, branched tx (popover), tempid, cardinality, legacy compat with rpc fns
; 2. wireframe all combinations
;      table with commands in rows
;      table with create new underneath
;      form with command in record
;      form with create new underneath
;      inline command, branched command
;      X legacy compat

; Rosie - intersting pages
; create admin (Rosie 1)
; deactivate district
; block sub from school
; edit pay rate
; Create Admin (Dustin's theoretical mega admin component)
; request interval editor

; 3. propose long-term notation for each of these Rosie user stories


; Dustin can do this, on monday Geoffrey will look and sync
