(ns ...
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql :refer [hfql]]))

#?(:clj (defn index-query [search type]
          (datomic.api/q '[:find [?e ...]
                           :where [?e :user/email]
                           hf/*$*])))

(s/fdef index-query :args (s/cat ::search string? ::type ref?) :ret (s/coll-of any?))

#?(:clj (defn sub-name [e]))

#?(:clj (defn rosie-type-options [rosie-index-search needle]
          [1 2 3 4 5 6 7]
          #_(remove
              ...
              [:admin/id
               :sub/id
               :school/id
               :parent/id
               ;:pod/id
               :district/id
               :pending-sub/id
               :sub-req.index/short-code])))

#?(:clj (defn rosie-type-by-id [e]
          4))

#?(:clj
   (defn rosie-type [e]
     (let [db hf/*$*
           !e (d/entity db e)]
       (cond (:admin/id !e) ::admin
             (:sub/id !e) ::sub
             (:school/id !e) ::school
             (:parent/id !e) ::parent
             (:pod/id !e) ::pod
             (:district/id !e) ::district
             (:pending-sub/id !e) ::pending-sub
             (:sub-req.index/short-code !e) ::sub-request))))

(p/defn render-index-query [V props]
  (dom/h1 "Find sub ...")
  ...)


(defn block-sub-from-school [sub school reason mode penalize]
  ...query
  (fn tx []
    `(tx ~sub ~school ~reason ~mode ~penalize)))

(p/defn App [db x]
  (hfql [hf/*$* db
         hf/*nav!* hf/*nav!*]
    {(index-query
       {(props type {::hf/options (rosie-type-options search .)})
        [:db/ident]}
       search)
     [:db/id
      sub-name
      :google/email
      {(props rosie-type-by-id {::hf/options (rosie-type-options . .)})
       [:db/ident]}
      rosie-type

      (popover (block-sub-from-school
                 sub
                 school
                 reason
                 (props mode {::hf/default (or mode :force-cancel)
                              ::hf/render checkbox-renderer
                              ::hf/options (block-mode-codes)
                              ::hf/option-label (let []
                                                  (dom/span ...))})
                 penalize)
               {::hf/tx (block-sub-from-school-tx sub school reason mode penalize)})


      (props (suber2.web.cmd/change-email-prompt (props id {::hf/hidden true})
                                                 email)
             {::hf/tx (suber2.web.cmd/change-email-tx id email)})

      ]})

  (dom/hr)
  (hf/popover "create-admin"
              (hfql (create-admin (d/tempid!) name email))
              ))

#?(:clj (defn change-email-tx [user email] nil))
(s/fdef change-email-tx :args (s/cat ::user string? ::email (s/and email? some?)))


#?(:clj (defn create-admin [admin name email] nil))
(s/fdef create-admin :args (s/cat ::admin string? ::name (s/and string? some?) ::email email?))

;::hf/render-as
;(defn hfql-internal-choose-render-fn [schema spec-registry {:as ast-node :keys [::hf/options]}]
;  (if (and type-ref options) typeahead-renderer)
;  )