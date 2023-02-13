(ns user.fiddle-pages
  (:require [clojure.spec.alpha :as s]
            [geoffrey.fiddle-effects :refer [submissions genders shirt-sizes submission-details
                                             submission gender shirt-size]]
            [hfdl.lang :refer [#?(:clj vars) #?(:clj debug) local2]]
            [hyperfiddle.api :as hf]
    ;; [hyperfiddle.server.ssr :as ui]
            [hyperfiddle.client.ui :as ui]
            [missionary.core :as m]
            [hyperfiddle.client.edn-view :as ev]
            [hyperfiddle.q2 :as q]
            [hyperfiddle.common.routes :as common-routes])
  #?(:cljs (:require-macros [hfdl.lang :refer [dataflow]]
                            [hyperfiddle.q2 :refer [hfql]])))

(s/fdef page-submissions :args (s/cat :needle string?))

(s/def :dustingetz/email string?)

(defn set-needle! [needle %]
  (reset! needle (.. % -target -value)))

(defn render-with-deep-input [e props]
  #?(:cljs
     (dataflow
       ~@(let [!needle (atom "")
               needle @(m/watch !needle)]
           [:div
            ;; (ui/new-input! needle (ui/set-input! !needle))
            [::selection e]
            [::options ~@(shirt-size (:db/ident e) needle)]]))))

;; (defn render-with-deep-input [e {:keys [>options]}]
;;   (dataflow
;;    ~@(let [!needle (atom "")
;;            id      (str (gensym))
;;            foo     (create-element "")
;;            needle  @(m/watch !needle)]
;;        (ui/div nil
;;         (ui/input {"value"       @needle
;;                    "list"        id
;;                    "placeholder" "needle"
;;                    "oninput"     (set-needle! needle)})
;;         (ui/datalist {"id" id}
;;          (reactive-for (fn [shirt-size]
;;                          (ui/option {"value" shirt-size}))
;;                        @>options))))))

(defn render-email [>v props]
  (prn 'render-email >v props)
  #?(:cljs
     (dataflow
       (let [v @>v]
         (prn "v" v)
         ~@[::hi (pr-str v)]))))

(defn render-text [a]
  #?(:cljs
     (dataflow
       ~@(do (ui/mount-component-at-node! "hf-ui-root" (ui/text ~a))
             ::done))))

(defn simple-email [needle]
  #?(:clj
     #_(dataflow @(render-text (hfql [{(submission "") [(:dustingetz/email #_#_::hf/render render-email)]}])))
     (render-text needle)))

(defn page-submissions [needle]
  #_(:cljs
     (dataflow
       (hfql
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
  #_(:cljs
     (dataflow
       (hfql
         [{(submission-details eid) [:db/id
                                     :dustingetz/email
                                     :dustingetz/shirt-size
                                     {:dustingetz/gender [:db/ident {(shirt-sizes dustingetz/gender) [:db/ident]}]}]}
          {(gender) [:db/ident]}]))))

(def fiddles
  #?(:clj (vars page-submissions page-submission-details simple-email)))

(defn fiddle-error [route error]
  #?(:clj (m/ap {:route route :error error})))

(defn get-fiddle [route]
  #?(:clj
     (if (seq? route)
       (if-some [[f & args] (seq route)]
         (if (symbol? f)
           (if-some [fiddle (fiddles (keyword "dustin.fiddle-pages" (name f)))]
             (try (apply fiddle args)
                  (catch Throwable _
                    (fiddle-error route :internal)))
             (fiddle-error route :not-found))
           (fiddle-error route :invalid-not-symbol))
         (fiddle-error route :invalid-empty))
       (fiddle-error route :invalid-not-seq))))


(def edn-view
  #?(:cljs
     (dataflow
       (let [route-request @common-routes/>route]
         (ev/set-editor-value! (ev/editor (ui/by-id "hf-edn-view-route") ui/change-route!) route-request)
         (ev/set-editor-value! (ev/editor (ui/by-id "hf-edn-view-output") {}) ~@@(get-fiddle route-request))
         nil))))

(def ui-view
  #?(:cljs
     (dataflow
       (let [route-request @common-routes/>route]
         (ev/set-editor-value! (ev/editor (ui/by-id "hf-edn-view-route") ui/change-route!) route-request)
         ;; (ev/set-editor-value! (ev/editor (ui/by-id "hf-edn-view-output") {}) ~@@(get-fiddle route-request))
         ~@@(get-fiddle route-request)
         nil))))

(def exports
  #?(:clj (merge q/exports
            (vars render-email render-text set-needle!
              render-with-deep-input reset! m/watch atom get-fiddle prn
              pr-str gender submission shirt-size inc q/hf-nav hf/->Input))))

(comment
  (require '[hfdl.lang :refer [local2 debug]])

(defnode page [needle]
  (hfql
    [{(submissions needle)
      [:db/id
       (:person/email ::hf/a (submission-detail db/id))
       {(:person/gender ::hf/options (genders))
        [:db/ident]}
       {(:person/shirt-size
          ::hf/options (shirt-sizes dustingetz/gender _))
        [:db/ident]}]}]))

  ((local2 (merge q/exports ui/exports exports (vars render-email))
           (debug sample (simple-email "a"))) prn prn)
  @sample
  )
