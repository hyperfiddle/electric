(ns dustin.y2022.hfql.hfql-command-user-stories
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros dustin.y2022.hfql.hfql-command-user-stories)))


(p/defn AdminEditor "existing and create new" [admin-in-route] ; todo can't unify to lexical scope here
  (hf/hfql
    {{admin ; ground
      {:options (admin-picklist-by-email email-search)
       :new-option (suber2.web.cmd.create/create-admin' ; prompt further for name!
                     (tempid!) :admin . email-search)}}
     [{suber-name {:link [:swinged.rosie/admin %]}}
      {swinged.rosie.account/suber-email {:tx (swinged.rosie.account/change-email-tx admin .)}} ; emit inline
      {{:security/role {:options (security-roles)}}
       [:db/ident]}]}))

(defn suber2.web.cmd.create/create' [e type name email]
  [{:db/id e
    :google/email email
    :admin/name name
    :google/display-name name}
   [:user.fn/set-normalized-email e email]])

(s/fdef suber2.web.cmd.create/create'
        :args (s/cat :e ::hf/ref?
                     :type keyword?
                     :name string?
                     :email string?))

(defn admin-picklist-by-email [email-search] ...)
(s/fdef admin-picklist-by-email :args (s/cat :email-search string?) :ret (s/coll-of any?))
