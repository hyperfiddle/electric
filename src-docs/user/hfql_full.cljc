(ns user.hfql-full
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests ! %]]
            [hyperfiddle.ui2 :as ui]
            [user.gender-shirt-size :refer [submissions genders shirt-sizes emails]]
            #?(:clj [hyperfiddle.q5 :refer [hfql]]))
  #?(:cljs (:require-macros [hyperfiddle.q5 :refer [hfql]]
                            [user.hfql-full :refer [page-submissions submissions-renderer
                                                    render-typeahead page-submissions-query]])))

(p/def submissions-renderer
  #'(dom/div
     (dom/h1 (dom/text "Submissions"))
     ~hf/value))

(p/defn render-typeahead []
  (binding [hf/render hf/sequenceM]
    (let [id      (str (gensym))
          options (::hf/options hf/props)]
      (dom/fragment
       (let [value' (p/$ ui/input-renderer2 {:dom.attribute/type "search"
                                             :dom.property/value (str (if (::hf/option-label hf/props)
                                                                        ~((::hf/option-label hf/props) ~hf/value)
                                                                        ~hf/value))
                                             :dom.attribute/list id})]
         (dom/element "datalist"
                      (dom/attribute "id" id)
                      (p/for [option ~options]
                        (dom/option #_(when (= ~hf/value option)
                                        (dom/attribute "selected" true))
                                    (dom/text ((::hf/option-label hf/props identity) option)))))
         value')))))

(p/defn page-submissions-query []
  (hfql {(submissions {(needle . ::hf/render render-typeahead
                               ::hf/options (emails needle2)
                               ::hf/option-label :dustingetz/email)
                       [:dustingetz/email]})
         [:dustingetz/email
          {(:dustingetz/gender . ::hf/options (genders)
                               ::hf/render render-typeahead
                               ::hf/option-label :db/ident)
           [(:db/ident . ::hf/as gender)]}
          {(:dustingetz/shirt-size . ::hf/options (shirt-sizes gender "")
                                   ::hf/option-label :db/ident
                                   ::hf/render  render-typeahead)
           [:db/ident]}]}) )

(p/defn page-submissions []
  (binding [hf/render ui/render]
    (dom/div (p/$ page-submissions-query))))

(tests
  (p/run (binding [hyperfiddle.q5/bypass-renderers true] (! (p/$ page-submissions-query))))
  % := '{(user.gender-shirt-size/submissions needle)
         [#:dustingetz{:gender #:db{:ident :dustingetz/female},
                       :email "alice@example.com",
                       :shirt-size #:db{:ident :dustingetz/womens-large}}
          #:dustingetz{:gender #:db{:ident :dustingetz/male},
                       :email "bob@example.com",
                       :shirt-size #:db{:ident :dustingetz/mens-large}}
          #:dustingetz{:gender #:db{:ident :dustingetz/male},
                       :email "charlie@example.com",
                       :shirt-size #:db{:ident :dustingetz/mens-medium}}]})

;; (hyperfiddle.rcf/enable! true)
;; (hyperfiddle.rcf/enable! false)
;; (shadow.cljs.devtools.api/compile :app)

;; * DONE Misplaced renderer
;;   CLOSED: [2021-08-25 Wed 16:39]
;;
;;   `::hf/render` on map key is misplaced to the next branch. If `foo` is
;;   cardinality many, the bar renderer will be placed on the p/for instead of
;;   the result of the p/for.
;;
;;   #+begin_src clojure
;;     {(foo ::hf/render bar) [:db/id]}
;;   #+end_src
;;
;; * DONE Basic gender/shirt-size example running in browser
;;   CLOSED: [2021-08-25 Wed 18:31]
;;
;;   Replaces the typeahead example
;;
;; * DONE parent-child deps [2/2]
;;   CLOSED: [2021-08-27 Fri 15:00]
;;
;; ** DONE Given an attr `a`, make it available in scope
;;    CLOSED: [2021-08-27 Fri 14:59]
;;
;;    - [ ] Lexical scope is not toposorted, but it’s an acceptable tradeoff for now.
;;
;;    - [ ] `a` must resolve to a flow, not to the value. Any references to a must join it.
;;
;; ** DONE Toposort the lexical scope
;;    CLOSED: [2021-08-27 Fri 14:59]
;;
;;    Raise an error at compile time in case of cycle.
;;
;;
;; * STARTED free variables
;;
;; * CANCELED +Path-based renderer+
;;
;;   Need to track the current hfql path.
;;
;;   #+begin_src clojure
;;     (let [path []]
;;       (let [path (conj path :root)]
;;         (let [path (conj path :branch)]
;;           (let [path (conj path :leaf)]))))
;;   #+end_src
;;
;;   Can be circumvented by specifying renderer manually at each point.
;;
;;
;; * DONE Schema-based renderer
;;   CLOSED: [2021-09-30 Thu 10:12]
;;
;; * DONE Spec extention
;;   CLOSED: [2021-09-30 Thu 10:12]
;;
