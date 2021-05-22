(ns dustin.fiddle-pages
  #?(:clj (:require [clojure.spec.alpha :as s]
                    [geoffrey.fiddle-effects :refer [submissions genders shirt-sizes submission-details
                                                     submission gender shirt-size]]
                    [hyperfiddle.q2 :as q]
                    [hfdl.lang :refer [dataflow vars system debug]]
                    [hyperfiddle.api :as hf]
                    [hyperfiddle.client.ui :as ui]
                    [missionary.core :as m]))
  #?(:cljs (:require [clojure.spec.alpha :as s]
                     [hyperfiddle.api :as hf]
                     [dustin.fiddle]
                     [missionary.core :as m]
                     [hyperfiddle.client.ui :as ui]))
  #?(:cljs (:require-macros [hfdl.lang :refer [dataflow vars]])))

(s/fdef page-submissions :args (s/cat :needle string?))

(s/def :dustingetz/email string?)


(defn render-with-deep-input [e props]
  (dataflow
    ~@(let [!needle (atom "")
            _ (hyperfiddle.client.ui/hack !needle)
            needle @(m/watch !needle)]
        [:div
         (ui/new-input! needle (ui/set-input! !needle))
         [::selection e]
         [::options ~@(shirt-size (:db/ident e) needle)]])))

(defn render-email [>v props]
  (prn 'render-email >v props)
  (dataflow
   (let [v @>v]
     (prn "v" v)
     ~@[::hi (pr-str v)])))

(defn render-text [>a]
  (dataflow
   (let [a @>a]
     ~@(do (ui/mount-component-at-node! "hf-ui-root" (ui/text ~a))
           ::done))))

(defn simple-email [needle]
  #?(:clj
     #_(dataflow @(render-text (q/hfql [{(submission "") [(:dustingetz/email #_#_::hf/render render-email)]}])))
     (render-text needle)))

(defn page-submissions [needle]
  #?(:clj
     (dataflow
       (q/hfql
         [{(submissions needle)
           [(:db/id ::hf/a (dustin.fiddle-pages/page-submission-details %)) ;; TODO expand sym
            (:dustingetz/email ::hf/render render-email)
            #_{(:dustingetz/gender ::hf/render render-with-deep-input)
             [:db/ident]}
            #_{((:dustingetz/gender %)
                ::hf/options (shirt-size dustingetz/gender _)
                ::hf/render ui/picklist #_render-gender)
               [:db/ident]}]}
          {(gender) [:db/ident]}]))))

(defn page-submission-details [eid]
  #?(:clj
     (dataflow
       (q/hfql
         [{(submission-details eid) [:db/id
                                     :dustingetz/email
                                     :dustingetz/shirt-size
                                     {:dustingetz/gender [:db/ident {(shirt-sizes dustingetz/gender) [:db/ident]}]}]}
          {(gender) [:db/ident]}]))))

(def fiddles (vars page-submissions page-submission-details simple-email))
(def exports (vars render-email render-text 
               render-with-deep-input reset! m/watch atom
               ui/picklist
               pr-str gender submission shirt-size inc q/hf-nav hf/->Input))

(comment
  (require '[hfdl.lang :refer [system debug]])
  (defn program [needle]
    (dataflow
      (q/hfql
        [{((submissions needle) ::hf/render ui/render-table)
          [:db/id
           :dustingetz/email
           {((:dustingetz/gender %)
             ::hf/render ui/picklist
             ::hf/options (shirt-sizes dustingetz/gender))
            [:db/ident]}]}])))

  ((system (merge q/exports ui/exports exports (vars ui/picklist ui/render-table render-text' render-email ui/render-row ))
     (debug sample (simple-email "a"))) prn prn)
  @sample
  )
