(ns dustin.hypermedia2
  (:require
    [clojure.spec.alpha :as s]
    [minitest :refer [tests]]))

(declare match hfql sub _ ... * ref?)

; 1. Graph substrate (schema, relations, queries)
; 2. Application-specific data-sync (page routing, hfql)
; 3. View progressive enhancement (link/iframe/button)



; The graph substrate is infinite comonad, you can just keep unfolding
; by applying arbitrary functions (queries, edge traversals).
; The specs are helpers so you can figure out what edge parameters make sense.
(def Suber-graph [])
; Perhaps we can constrain the legal functions? Or allow any function, constrain by classpath?

; edges (pull symbols)
(defn suber-name [sub])
(defn sub-locations [sub])
(defn sub-quals [sub])
(defn sub-metrics [sub])

; pages (special edges)
(defn sub-display [sub])
(s/fdef sub-display :args (s/cat ::sub (s/and some? ref?)) :ret (s/keys :req [::locations ::quals]))
(defn sub-blocked-list [sub])
(s/fdef sub-blocked-list :args (s/cat ::sub (s/and some? ref?)))
(defn sub-requests [sub])
(s/fdef sub-requests :args (s/cat :sub ref? ::since inst? ::status (s/nilable ref?) ::school (s/nilable ref?)))
(defn sub-request [sub])
(s/def ::sub-request (s/keys :req [:sub-req.index/start :sub-req.index/end :swinged.rosie.sub-req/ttstart
                                   :sub-req/status ::rosie/name :school/name :sub-req/contact-name
                                   :swinged.rosie.sub-req/class :sub-req/id]))
(defn rosie-entity-history [e before attr])
(s/fdef rosie-entity-history
  :args (s/cat :entity (s/and some? ref?) :before inst? :attr ref?)
  :ret (s/coll-of (s/keys :req [::a ::who ::v ::op ::memo])))

(defn sub-rename [sub])

(defn ^::hf/tx school-requested-block [sub school block-reason mode penalize]
  (set! hf/*tx [`(suber.web.cmd/school-requested-block ~school ~sub ~mode ~block-reason ~penalize)])
  nil)
(s/fdef school-requested-block :args (s/cat
                                       :sub (s/and some? ref?)
                                       ::school (s/and some? ref?)
                                       ::block-reason (s/and some? ref?)
                                       ::block-mode (s/and some? keyword?)
                                       ::penalize? boolean?))

(defn district-requested-block [sub district block-reason]
  (set! hf/*tx [`(suber.web.cmd/district-requested-block ~district ~sub ~block-reason #_~mode #_~penalize)])
  nil)
(s/fdef district-requested-block :args (s/cat
                                         :sub (s/and some? ref?)
                                         ::district (s/and some? ref?)
                                         ::block-reason (s/and some? ref?)
                                         #_#_::block-mode (s/and some? keyword?)
                                         #_#_::penalize? boolean?))

(defn alias-to [e])

(defn admin-profile [admin])

; options queries (special edges)
(defn all-locations [needle])
(s/fdef all-locations :args (s/cat :needle string?))

(defn esub-region-masterlist [needle])
(s/fdef esub-region-masterlist :args (s/cat :needle string?))

(defn all-subs [needle])
(s/fdef all-subs :args (s/cat :needle string?))

(defn event-reason-codes-picklist [needle])
(s/fdef event-reason-codes-picklist :args (s/cat :needle string?))

(defn school-picklist [needle])
(s/fdef school-picklist :args (s/cat :needle string?))

(defn district-picklist [needle])
(s/fdef district-picklist :args (s/cat :needle string?))

(defn unlink-mode-options [] [:force-cancel :leave-commitments])

(defn index [search type])

; helpers (edge too!)
(defn rosie-type [e])
(defn rosie-type-options [needle])

(defn display-school [school])
(defn display-district [district])
(defn parent-detail [parent])

; Routing
(def route `(sub-display 123))                              ; restricted to whitelisted data access patterns
#_(def route `{(sub-display [:sub/id 123]) [:sub/name {(rosie-entity-history % _ _) [:db/id]}]})

`{sub-display [:sub/id
               suber-name
               {sub-metrics [:db/id]}
               (sub-locations % _)
               '(rosie-entity-history % _ _)]}

(defmacro hfql [ast]
  (println ast)
  #_ast)

(def Rosie
  ; Keys are routable pages
  ; HF server will eagerly join the graph to this depth, and present links as lazy streams
  `{index                       ~(fn [e]
                                   `[:db/id
                                     :google/email
                                     :google.index/normalized-email
                                     ~@(match (rosie-type e)
                                         ::admin `[:admin/name :admin/id 'admin-profile]
                                         ::sub `[:sub/name :sub/id 'sub-display]
                                         ::school [:school/id 'display-school]
                                         ::district `[:district/id 'display-district]
                                         ::parent `[:parent/id 'parent-detail]
                                         _ [])])
    sub-display                 [:sub/id
                                 :cognito/email
                                 :google/email
                                 :sub/phone
                                 :sub/phone-confirmed
                                 :sub/about
                                 :sub/photo
                                 {:esub/region [:region/id]}
                                 {:sub/priority [:sub-priority/id]}
                                 {:sub/experience [*]}
                                 :sub.index/cancel-rate
                                 suber-name
                                 sub-locations
                                 sub-quals
                                 {sub-metrics [:sub.metrics/feedback-score
                                               :sub.metrics/reliability-score]}

                                 ; hyperlinks
                                 'sub-blocked-list
                                 'sub-requests
                                 '(rosie-entity-history % _ _) ; can nav then prompt for unfilled params

                                 ; lazy queries (select options, popovers) reduce to hyperlinks because
                                 ; the rich client chooses presentation
                                 'sub-rename
                                 '(school-requested-block % _ _ _ _)
                                 '(district-requested-block % _ _)
                                 'alias-to
                                 '(all-locations _)
                                 '(esub-region-masterlist _)
                                 '(all-subs _)]
    sub-requests                [:sub-req.index/start
                                 :sub-req.index/end
                                 :school/name
                                 :sub-req/id
                                 'sub-request]
    sub-request                 [:sub-req/grade
                                 :sub-req/subject
                                 :sub-req/memo
                                 {:sub-req/status [:db/ident]}
                                 :sub/id
                                 'sub-display]
    all-locations               [:db/ident]
    rosie-entity-history        (fn [& [e before attr]]
                                  `[*
                                    :db/id
                                    ~@(match (rosie-type e)
                                        ::admin [:admin/name
                                                 :admin/id
                                                 'admin-profile]
                                        ::sub [:sub/name
                                               :sub/id
                                               'sub-display]
                                        _ nil)])
    admin-profile               [*]
    sub-blocked-list            [*]
    rosie-entity-history        [*]
    sub-rename                  [*]
    school-requested-block      ['event-reason-codes-picklist
                                 'school-picklist
                                 'unlink-mode-options]
    district-requested-block    [*
                                 'event-reason-codes-picklist
                                 'district-picklist]
    alias-to                    [*]
    esub-region-masterlist      [:region/id]
    all-subs                    [:sub/id
                                 ::rosie/name]
    event-reason-codes-picklist [:db/ident]
    school-picklist             [:school/name
                                 :school/id]
    district-picklist           [:district/name
                                 :district/id]
    })

; Todo: hf/defaults - i think this works as is
; Todo: hf/view-defaults - (links for the prompt)

(comment
  ~sub-requests
  @sub-requests
  `sub-requests
  'sub-requests

  ... sub-requests
  ! sub-requests
  ? sub-requests
  = sub-requests
  / sub-requests
  | sub-requests
  + sub-requests
  - sub-requests
  -> sub-requests
  => sub-requests
  <- sub-requests
  > sub-requests
  $ sub-requests
  % sub-requests
  * sub-requests
  & sub-requests                                            ; legal sometimes
  _ sub-requests

  Α α, Β β, Γ γ, Δ δ, Ε ε, Ζ ζ, Η η, Θ θ, Ι ι, Κ κ, Λ λ, Μ μ, Ν ν, Ξ ξ, Ο ο, Π π, Ρ ρ, Σ σ/ς, Τ τ, Υ υ, Φ φ, Χ χ, Ψ ψ, Ω ω

  ; unicode too



  ;illegal
  ; ^ sub-requests ^sub-requests
  ; # sub-requests #sub-requests
  ; \ sub-requests
  ; (quote (,,, 1)) := '(1)
  ; () [] {}
  )