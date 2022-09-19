(ns dustin.hfql.hfql-router
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as d]
            [hyperfiddle.api :as hf :refer [hfql]]
            [hfdl.lang :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :as rcf :refer [tests tap %]]
            [missionary.core :as m]

            [user.gender-shirt-size :refer [genders shirt-sizes submissions submission]]
            ))

; What is a page (route)
; How does HFQL connect to a page / route

(p/defn submissions-page [route-params]
  (let [[needle gender needle2] route-params]
    (dom/div
      (hfql
        {(submissions needle)
         [(:dustingetz/email ::hf/a (submission-page hf/e email))
          :dustingetz/email1
          {(:dustingetz/gender ::hf/options (genders)) [:db/id :db/ident :dustingetz/type]}
          {(:dustingetz/shirt-size ::hf/options (shirt-sizes gender .)) [:db/ident]}]})
      (hfql
        {(shirt-sizes gender needle2) [:db/ident]}))))

(p/defn submission-page [route-params]
  (let [[e] route-params]
    (hfql
      [{(submission e) [:db/id
                        :dustingetz/email
                        {(:dustingetz/gender ::hf/options (genders)) [:db/id :db/ident :dustingetz/type]}
                        {(:dustingetz/shirt-size ::hf/options (shirt-sizes gender .)) [:db/ident]}]}])))

(def router (atom "/"))
(p/defn entrypoint []
  (let [route (parse-route ~(m/watch router))]
    (case route
      `submissions-page (apply submissions-page route)
      `submission-page (apply submission-page route))))
