(ns dustin.fiddle-pages
  #?(:clj (:require [clojure.spec.alpha :as s]
                    [dustin.fiddle :refer [submissions genders shirt-sizes submission-details
                                           submission gender shirt-size]]
                    [hyperfiddle.q :as q]
                    [hfdl.lang :refer [dataflow vars]]
                    [hyperfiddle.api :as hf]
                    [hyperfiddle.client.ui :as ui]
                    [missionary.core :as m]))
  #?(:cljs (:require [clojure.spec.alpha :as s]
                     [hyperfiddle.api :as hf]
                     [dustin.fiddle]
                     [missionary.core :as m]))
  #?(:cljs (:require-macros [hfdl.lang :refer [dataflow vars]])))

(s/fdef page-submissions :args (s/cat :needle string?))

(s/def :dustingetz/email string?)

(defn render-email [e props]
  (dataflow
    ~@[::hi (pr-str ~@(q/hf-nav :db/id [:dustingetz/email e]))]))

(defn render-with-deep-input [e props]
  (dataflow
    ~@(let [!needle (atom "")
            _ (hyperfiddle.client.ui/hack !needle)
            needle @(m/watch !needle)]
        [:div
         [:input {:value (hf/->Input needle)
                    #_#_:on-change (fn [%] (reset! !needle (.-value (.-target %))))}]
         [::selection e]
         [::options ~@(shirt-size (:db/ident e) needle)]])))

(defn page-submissions [needle]
  #?(:clj
     (dataflow
       (q/hfql
         [{(submission needle)
           [(:db/id ::hf/a (dustin.fiddle-pages/page-submission-details %)) ;; TODO expand sym
            (:dustingetz/email ::hf/render render-email)
            {(:dustingetz/gender ::hf/render render-with-deep-input)
             [:db/ident]}
            #_
            {((:dustingetz/gender %)
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
  ((system (merge q/exports (vars ui/picklist)) (debug sample (program ""))) prn prn)
  @sample
  )

(def fiddles (vars page-submissions page-submission-details))
(def exports (vars render-email
                   #_render-gender
                   render-with-deep-input reset! m/watch atom
                   ui/picklist
                   pr-str gender submission shirt-size inc q/hf-nav hf/->Input))
