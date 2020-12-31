(ns dustin.rosie2
  (:require
    [minitest :refer [tests]]))

(declare match hfql sub _ ... *)

; 1. Graph substrate (schema, relations, queries)
; 2. Application-specific data-sync (page routing, hfql)
; 3. View progressive enhancement (link/iframe/button)
; Which points are page-routable?

; Default routes?
; Client in charge of link traversal, or not?
; GraphQL request?

; edges
(defn suber-name-kv [sub])
(defn sub-locations-kv [sub])
(defn sub-quals-kv [sub])
(defn sub-metrics-kv [sub])

; pages (special edges)
(defn sub-display [sub])
(defn sub-blocked-list [sub])
(defn sub-requests [sub])
(defn sub-request [sub])
(defn rosie-entity-history [e before attr])
(defn sub-rename [sub])
(defn school-requested-block [school])
(defn district-requested-block [district])
(defn alias-to [e])
(defn admin-display [admin])

; options queries (special edges)
(defn all-locations [])
(defn esub-region-masterlist [])
(defn all-subs [])

; helpers (edge too!)
(defn rosie-type [e])

; What is a route?
(def route `(sub-requests sub))
(def route `{(sub-display [:sub/id ...]) [...]})
; To render that point, you need the specs to fill in the URL if not already
; If you route to an attribute, we prompt for an entity (boring)
; If you route to a function, we prompt for the params
;     What if the fn appears in the graph at many points?
; By default we will use the default pull/page structure (whitelisted access pattern)
; The client maybe can override this
; are the specs the things that are routable?

; The graph substrate is infinite comonad, you can just keep
; applying arbitrary functions (queries).
(def Suber-graph)                                           ; nothing to do

(def Rosie
  ; default structure and pull depth?
  ; routable points in graph?
  {`sub-display          [:sub/id
                          :cognito/email
                          :google/email
                          :sub/phone
                          :sub/phone-confirmed
                          :sub/about
                          :sub/photo
                          {:esub/region [:region/id]}
                          {:sub/priority [:sub-priority/id]}
                          {:sub/experience ['*]}
                          :sub.index/cancel-rate
                          ; if they produce scalars, render as pulledtree
                          ; if they produce identity, render as link bc no pull?
                          suber-name-kv
                          sub-locations-kv
                          sub-quals-kv
                          {sub-metrics-kv [:sub.metrics/feedback-score
                                           :sub.metrics/reliability-score]}]
   `sub-requests         [:sub-req.index/start
                          :sub-req.index/end
                          :school/name
                          :sub-req/id
                          sub-request]
   `sub-request          [:sub-req/grade
                          :sub-req/subject
                          :sub-req/memo
                          {:sub-req/status [:db/ident]}
                          :sub/id
                          ~sub-display]
   `all-locations        [:db/ident]
   `rosie-entity-history (fn [e before attr]
                           `['*
                             :db/id
                             ~@(match (rosie-type e)
                                 ::admin [:admin/name
                                          :admin/id
                                          {admin-display [...]}]
                                 ::sub [:sub/name
                                        :sub/id
                                        sub-display]
                                 _ nil)])
   `admin-display        ['*]
   })
