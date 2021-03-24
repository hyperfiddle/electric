(ns dustin.hypermedia3)


(defn ^::hf/fiddle admin-requests [admin since] (d/q ...))
(s/fdef admin-requests :args (s/cat :admin _ :since _))

(defn sub-request [e])

(defn create-admin-tx [admin name email roles]
  [[:db/add admin :admin/name name]])

(defn create-admin-page [])
; spec :args

(defmethod hf/render :roles []
  (picklist-renderre
    {::hf/options `(swinged.rosie.admins/security-roles nil)}
    ))

(comment

  (hfql {(create-admin) [:admin
                         :nanme
                         :email
                         {:roles [:db/ident]}
                         (roles-options) [:db/ident]]})

  (hfql
    {(admin-requests admin since) [:start
                                   :end
                                   :sub-name
                                   {:status [:db/ident]}

                                   :sub-req/id

                                   ; <a href="/sub-reqest/${sub-req/id}" />
                                   ;
                                   ; <form action="/sub-reqest/${sub-req/id}"
                                   ;  method="POST">
                                   ;   <input type="id" name="sub-req/id">
                                   ;   < utton type'Sbumit">
                                   ; </form>

                                   `(sub-request ~sub-req/id)
                                   (sub-request ~sub-req/status)

                                   ]}
    )

  )