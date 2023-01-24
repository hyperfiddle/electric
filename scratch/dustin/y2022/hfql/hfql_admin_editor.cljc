(ns dustin.y2022.hfql.hfql-admin-editor
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  #?(:cljs (:require-macros dustin.y2022.hfql.hfql-admin-editor)))

; Insight: popover (push scope) can be inferred based on need

(p/defn AdminEditor "existing and create new" [admin] ; route cycle
  (hf/hfql
    {{{admin {:options (admin-picklist search)
              :new-option (suber2.web.cmd.create/create-admin' ; prompt further for name and email! so push scope
                            (tempid!) :admin . .) ; cant reuse unstructured search
              :on-select (hf/replace-route! [:swinged.rosie/admin %])}}
      [{suber-name {:link [:swinged.rosie/admin %]}}]}

     [{suber-email {:tx (change-email-tx admin suber-email)}} ; can emit inline, no need to push scope
      {{:security/role {:options (security-roles)}}
       [:db/ident]}]}))

(defn suber2-create-admin [e type name email]
  [{:db/id e
    :google/email email
    :admin/name name
    :google/display-name name}
   [:user.fn/set-normalized-email e email]])

(s/fdef suber2-create-admin
        :args (s/cat :e ::hf/ref?
                     :type keyword?
                     :name string?
                     :email string?))

(defn admin-picklist-by-email [email-search] ...)
(s/fdef admin-picklist-by-email :args (s/cat :email-search string?) :ret (s/coll-of any?))


; button popovers are for commands
; Create-admin is not a command, it flattens into the admin editor
; Block-sub is a command

; Need a popover to collect additional information and to branch/review before commit