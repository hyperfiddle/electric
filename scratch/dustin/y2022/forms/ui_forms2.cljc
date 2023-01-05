(ns ui-forms2
  (:require [clojure.core.protocols :as ccp :refer [nav]]
            [clojure.datafy :refer [datafy]]
            #?(:clj [contrib.datomic-contrib :as dx])
            [contrib.str :refer [pprint-str]]
            #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui]
            [hyperfiddle.popover :refer [Popover]])
  (:import [hyperfiddle.photon Pending]
           [missionary Cancelled])
  #?(:cljs (:require-macros ui-forms2)))

;(defn valid-label? [txn]) -- what is the type? when does validation happen? after hydrate
(p/def into-tx) ; merge-command. server

;#?(:clj (defn nav [db e a] (get entity a)))

; Stage is a graph
; local state comes from it
(p/def !stage) ; server

(defmacro input2 [x a props]
  `(let [x# ~x
         a# ~a
         v# (ui/input (datafy (nav x# a# (a# x#))) ~props)
         e# (p/server (dx/identify x#))]
     (p/server (swap! !stage into-tx [[:db/add e# a# v#]])
               nil)
     nil))

(defmacro input3 [x a props]
  `{::ui/value (ui/input (datafy (nav x# a# (a# x#))) ~props)})

(p/defn LabelForm [e]
  (p/server
    (let [record (d/pull hf/db label-form-spec cobbblestone)]
      (p/client
        (merge
          (dom/h1 "Label editor for: " (p/server (:label/name record)))
          (dom/dd
            (dom/dt "id")
            #_(dom/dd (input2 record :db/id {::dom/disabled true})) ; Impure CT
            (dom/dd {::ui/value (ui/input (:db/id record) {::dom/disabled true})}) ; Pure CT

            (dom/dt "id")
            (dom/dd
              ; dd can bundle the merge, the cost is interpreting strings vs command structure
              {:db/id "123"}
              (input2 record :db/id) ; leo style.
              {:db/ident "asdfasdf"})

            (dom/dd (do ;
                      (merge ; cost is explicit merge, which is not that bad
                        {:db/id "123"}
                        (input2 record :db/id) ; leo style.
                        {:db/ident "asdfasdf"})))

            (dom/dt "gid" #_(dom/text "gid")) ; loss of syntax
            (dom/dd #_(ui/uuid (:label/gid record) {::dom/disabled true}))

            (dom/dt "name")
            (dom/dd (input2 record :label/name {}))

            (dom/dt "startYear")
            (dom/dd #_(input2 record :label/startYear {}))

            ;{:label/type [:db/ident]}

            ))
        (dom/pre (pprint-str record)))

      #_(when (valid? record) txn)
      )))