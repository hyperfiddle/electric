(ns create-admin-1
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]))

(defn create-admin-query [admin name email] (tempid))
(s/fdef create-admin-query :args (s/cat :admin ref? :name string? :email string?))

(defn tempid [] "-1")

; create admin
(p/defn PopoverBody []
  (hf/hfql
    (props (create-admin-query (tempid) name email) {:render ...
                                                     :tx (suber.web/create-admin' admin :admin name email)})

    ))

(p/defn IndexPage []
  (Popover. "create-admin" PopoverBody))

(p/defn IndexPage []
  (let [admin (tempid)]
    (hf/hfql (props (create-admin-query admin name email) {:popover-anchor-label "create-admin"
                                                           :render ...
                                                           :tx (suber.web/create-admin' admin :admin name email)}))

    (hf/hfql {(props (create-admin-query admin name email) {:popover-anchor-label "create-admin"
                                                            :render ...
                                                            :tx (suber.web/create-admin' admin :admin name email)})
              [:db/id ...]}))



  (hf/hfql {(tempid)

            [:db/id

             (props (create-admin-query admin name email) {:popover-anchor-label "create-admin"
                                                           :render ...
                                                           :tx (suber.web/create-admin' admin :admin name email)})

             (props (create-admin-query admin name email) {:popover-anchor-label "create-admin"
                                                           :render ...
                                                           :tx (suber.web/create-admin' admin :admin name email)})

             (props (create-admin-query admin name email) {:popover-anchor-label "create-admin"
                                                           :render ...
                                                           :tx (suber.web/create-admin' admin :admin name email)})]})

  )