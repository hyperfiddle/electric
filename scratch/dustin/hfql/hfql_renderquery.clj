(ns dustin.hfql.hfql-renderquery
  (:require [geoffrey.fiddle-effects :refer [shirt-sizes gender submission]]
            [geoffrey.hfql.no-join :refer [hfql hf-nav]]
            [hfdl.impl.trace :refer [system debug]]
            [hfdl.lang :refer [dataflow]]
            [hyperfiddle.api :as hf]
            [hfdl.lib :refer [reactive-for]]
            [missionary.core :as m]))

;; (defn dom-form [>form props continue]
;;   (m/ap (println "form-mount props: " props)
;;         (let [x @>form]
;;           (continue x))))

;; (defn hiccup-abstract-form [>v props]
;;   (dataflow
;;     (println "form-mount props: " props)
;;     [:form props
;;      (for [[_k >v] @>v]
;;        @>v)]))

;;;(defn hiccup-form-empty [>form props]
;;  ; skip continuations, do no more work, query no deeper, sample no more
;;  (dataflow [:div]))

;;(defn codemirror-form [>form props]
;;  (dataflow ))

;; ;; Should this be a macro since map keys are always known statically?
;; (defn hiccup-hardcoded-form [v props] ;; v = {:db/id Flow<>}
;;   (let [renderf (fn [k v]
;;                   [:div
;;                    [:label k]
;;                    [:input {:type "text" :value v}]])]
;;     (apply m/latest #(into [:form props] (map renderf (keys v) %&))
;;            (vals v))))

;;(defn name' [>x _props] (dataflow (name @>x)))
;; (let [renderf (fn [_k v] v)]
;;   (apply m/latest (fn [& args]
;;                     (println 'form-mount)
;;                     (into [:form props] (map renderf (keys v) args)))
;;          (vals v)))

;; (m/latest (fn [v]
;;             [:div
;;              ;; [:label k]
;;              [:input (assoc props :value v)]])
;;           >v)

(defn run-effect! [f]
  (m/relieve {} (m/ap (f))))

(defmacro render-field [#_k >v props]
  `(let [a# @(run-effect! #(println 'field-mount))]
     [:div
      ;; [:label k]
      [:input (assoc ~props :value @~>v, :parent a#)]]))

(defmacro render-form [v props] ;; v = {:db/id Flow<>}
  (let [children (map (fn [[k >v]]
                        `[~k @~>v])
                      v)]
    `(let [a# @(run-effect! #(println 'form-mount))]
       (into [:form (assoc ~props :parent a#)] ~@children)))
  #_`(let [a @(run-effect! #(println 'form-mount))]
    (into [:form (assoc ~props :parent a)]
          @(reactive-for (fn [[_k >v]] (dataflow @>v)) (unquote ~v)))))

(comment
  (def dag
    (dataflow
     (let [x (hfql
              {((shirt-size :dustingetz/male) ::hf/render render-form)
               [(:db/id ::hf/render render-field)
                (:db/ident ::hf/render render-field)]})]
       (-> (get-in x ['(dustin.fiddle/shirt-size :dustingetz/male)])
           (deref)))))

  ((system (debug sampler dag)) prn prn)
  @sampler
;; => [:form
;;     {}
;;     [:div [:input {:value 3}]]
;;     [:div [:input {:value :dustingetz/mens-small}]]]

  ; println form-mount
  ; println field-mount
  := [:form [:div [:input]] [:div [:input]]]
  )


(comment


  (defn render-uppercase [val _props]
    (dataflow
     (let [a @(run-effect! #(println 'run2!))]
       [a (clojure.string/upper-case val)])))

  (clojure.tools.analyzer.jvm/macroexpand-all
   '(hfql {((submission needle) ::hf/render render-form)
           [(:dustingetz/email ::hf/render render-uppercase)
            {(:dustingetz/gender ::hf/options (gender)) [:db/ident]}
            {(:dustingetz/shirt-size ::hf/options (shirt-sizes (:dustingetz/gender %))
                                     ::hf/render simple-picklist)
             [:db/ident]}]})
   (clojure.tools.analyzer.jvm/empty-env))

  (defn render-form [val _props] #_{:dustingetz/shirt-size Flow
                                    :dustingetz/gender     Flow
                                    :dustingetz/email      Flow}
    (dataflow
     (let [a @(run-effect! #(println 'run1!))]
       [:div a @(get val :dustingetz/shirt-size)])))

  (defn simple-picklist [value {::hf/keys [options] :as props}]
    (dataflow
     (let [!input (atom …)
           a @(run-effect! #(println 'picklist-mount))]
       [:div
        [:input {:type :text :on-change #(reset! !input (.. % -target -value) )}]
        (into [:select {:value value, :parent a}]
              (for [option (options @!input)] ;; should this be a deref or function
                [:option ! (hf-nav :db/ident option)]))])))

  (defn shirt-sizes* [ids]
    )

  (def !needle (atom ""))
  (def >needle (m/watch !needle))
  (def dag
    (dataflow
     (let [needle @>needle
           x      @(hfql {((submission needle) ::hf/render render-form)
                          [(:dustingetz/email ::hf/render render-field :my-prop true)
                           {(:dustingetz/gender ::hf/options (gender)) [:db/ident]}
                           {(:dustingetz/shirt-size ::hf/options (shirt-sizes @(hf-nav :dustingetz/gender %) _) ;; need to pull here
                                                    ::hf/render simple-picklist)
                            [:db/ident]}]}) ]
       @(get x `(submission "")))))

  ((system (debug sampler dag)) prn prn)
  @sampler

  )
