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

(defn set-needle! [needle %]
  (reset! needle (.. % -target -value)))

;; * Leo: different idea about RT
;;
;; **Problem: how to introduce a new identity in a DF expression?**
;;
;; Ex:
;; - js/document.createElement
;; - clojure.core/atom
;; - gensym
;; **-> we don't want them to be re-used.**
;;
;; what does defines an apply node identity? apply nodes has a heap, the heap is
;; the identity. does the place in the AST is the identity? does let introduces
;; identities?
;;
;; Solutions:
;; 1. [Leo]: we write a macro, generating a unique sym at compile time. Instead
;;    of calling a non-rt fn we call a wrapper that proxies and discards the
;;    result at runtime. Each time the macro is called, the call is guaranteed
;;    to have a unique identity.
;;
;;    ```clojure
;;    (dataflow
;;     (id (atom 0)) ; id defines the identity (allocates a new one)
;;     (id (atom 0)) ; not the same identity as above
;;     ))
;;     ```
;;
;; 2. [Leo]: Remove RT, so the only way to re-use nodes (share them) is via let.
;;    Each expr has its own identity. Let propagates identities.
;;
;;    #+begin_src clojure
;;    ;; With let propagating identites
;;    (dataflow
;;     (let [a (atom 1)]
;;       (+ (deref a) (deref a)))) ; shared, same `a`, not the same `(deref a)`
;;
;;    ;; is different from
;;    (dataflow (+ @(atom 1) @(atom 1))) ; nothing shared
;;    #+end_src
;;
;;    One downside is we lose re-use across frames. So if we spawn multiple
;;    dataflows with the same structure, we are going to duplicate nodes having
;;    the same behavior.
;;
;;    #+begin_src clojure
;;    (defn new-integer [x] (java.lang.Integer. x))
;;
;;    (dataflow
;;     (+ (new-integer. 1) ; new is special case of `apply` with a new identity
;;        (new-integer. 1)))
;;
;;    (dataflow
;;     (let [a (atom. 1)
;;           sym (gensym.)
;;           elem (create-element. "div")]
;;       (+ @a @a)))
;;    #+end_src


(defn render-with-deep-input [e props]
  (dataflow
   ~@(let [!needle (atom "")
           _ (hyperfiddle.client.ui/hack !needle)
           needle @(m/watch !needle)]
       [:div
        (ui/new-input! needle (ui/set-input! !needle))
        [::selection e]
        [::options ~@(shirt-size (:db/ident e) needle)]])))

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
(def exports (vars render-email render-text set-needle!
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
