(ns user.gender-shirt-size
  (:require [clojure.spec.alpha :as s]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests ! %]]
            [missionary.core :as m])
  #?(:cljs (:require-macros [user.gender-shirt-size :refer [genders gender shirt-sizes submissions submission
                                                            page-submissions submissions-renderer render-string render-typeahead render-input]]
                            [hyperfiddle.q4 :refer [hfql]])))

(p/defn genders []
  (into [] ~ (hf/q '[:find [?e ...] :where [_ :dustingetz/gender ?e]])))

(tests
  (def dispose (p/run (! (p/$ genders))))
  % := [:dustingetz/male :dustingetz/female]
  (dispose))

(p/defn gender []
  (first (p/$ genders)))

(tests
  (def dispose (p/run (! (p/$ gender))))
  % := :dustingetz/male
  (dispose))

(p/defn shirt-sizes [gender needle]
  (sort
   ~(hf/q '[:in $ % ?gender ?needle
            :find [?e ...]
            :where
            [?e :dustingetz/type :dustingetz/shirt-size]
            [?e :dustingetz/gender ?gender]
            [?e :db/ident ?ident]
            (hyperfiddle.api/needle-match ?ident ?needle)]
          hf/rules gender (or needle ""))))

;; (datascript.core/touch (datascript.core/entity hf/*$* 5))

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?) :ret sequential?)

(p/defn submissions [needle]
  (sort
    ~(hf/q '[:find [?e ...]
          :in $ % ?needle
          :where
          [?e :dustingetz/email ?email]
          (hyperfiddle.api/needle-match ?email ?needle)]
       hf/rules (or needle ""))))

(s/fdef submissions :args (s/cat :needle string?)
        :ret (s/coll-of (s/keys :req [:dustingetz/email
                                      :dustingetz/email1
                                      :dustingetz/gender
                                      :dustingetz/shirt-size])))

(p/defn submission [needle] (first (p/$ submissions needle)))

(tests
  (hfdl.lang/run (! (p/$ user.gender-shirt-size/submission "")))
  %)

(s/fdef submissions :args (s/cat :needle string?) :ret sequential?)

(tests
  (def dispose
    (p/run
      (! (p/$ submissions ""))
      (! (p/$ submissions "example"))
      (! (p/$ submissions "b"))))
  % := [9 10 11]
  % := [9 10 11]
  % := [10]
  (dispose))

(comment
  (p/defn ref2 [v {::hf/keys [options]} needle]
    (dom/select v (p/for [x (options "")] (dom/option x))))

  ;(defmacro ref3 [v {::hf/keys [options]}]
  ;  `(let [gender nil
  ;         needle ""]
  ;     (dom/select ~v (p/for [x ~options] (dom/option x)))))

  (defmacro auto-render [v props]
    ;(let [!needle (atom "")])
    `(dom/div
       (dom/h1 "submissions")
       #_(let [args
               (dom/form
                 (dom/field (dom/span "needle") (dom/input needle))
                 (dom/field (dom/span "needle2") (dom/input needle2))
                 (dom/field (dom/span "needle3") (dom/input needle3)))]) ; in scope
       (let [xs ~v #_(v needle)]                            ; assume needle in scope
         (dom/table
           (p/for [{:keys [:db/id
                           :dustingetz/email
                           :dustingetz/gender
                           :dustingetz/shirt-size]} xs #_(xs ~(m/watch !needle))]
             (dom/tr
               (dom/td (str id))
               (dom/td email)
               (dom/td (pr-str gender))
               (dom/td (pr-str shirt-size))))))))

  (p/defn render-text [x opts]
    (dom/input x))

  (p/defn page-submissions []
    (hfql
     [{((submissions "") ::hf/render auto-render)
       [:db/id
        (:dustingetz/email ::hf/render render-text)
        {(:dustingetz/gender ::hf/options (genders) ::hf/render ref2) [:db/ident]}
        {(:dustingetz/shirt-size ::hf/options (shirt-sizes :dustingetz/male "")
                                 ::hf/render ref2) [:db/ident]}]}]))
  )

(p/def submissions-renderer
  #'(dom/div
     (dom/h1 (dom/text "Submissions"))
     (let [[_ value] ~hf/value] ;  join here for the free input to render in the right place
       (dom/table
        (dom/thead
         (dom/tr
          (dom/th (dom/text "email"))
          (dom/th (dom/text "gender"))
          (dom/th (dom/text "shirt-size"))))
        (dom/tbody
         ;; (dom/text (p/for [v value] ~v))
         (p/for [e (p/for [v value] ~v)] ; Flow [Flow] => Flow [a] => [a]
           (dom/tr
            (dom/td ~(:dustingetz/email e))
            (dom/td ~(:dustingetz/gender e))
            (dom/td ~(:dustingetz/shirt-size e)))))))))

(p/defn render-string []
  (let [value ~hf/value]
    #'(dom/input (dom/attribute "value" value))))

;; FIXME inpput and datalist should be declared co-located. For that options
;; need to be a partial such that we can pass the input value stream to the
;; options inside of the render. It would make HFQL simpler, removing the
;; automatic input rendering mechanism.

(p/defn render-typeahead []
  (let [value   hf/value
        options (::hf/options hf/props)]
    #'(let [value ~value
            [list-ids options] ~options]
        (prn "OPTIONS" list-ids options)
        (if (seq list-ids)
          (p/for [[_idx list-id] (sort-by key list-ids)]
            (dom/element "datalist"
                         (dom/attribute "id" list-id)
                         (p/for [option options]
                           (prn "OPTION" option)
                           (dom/option (dom/text ~(hf/nav option :db/ident))))))
          (dom/fragment
           (dom/select
            (p/for [option options]
                (dom/option (when (= value option)
                              (dom/attribute "selected" true))
                            (dom/text ~(hf/nav option :db/ident)))))))
          value)))

(p/defn render-input [list-id f arg-index]
  (let [initial-value ""]
    (dom/input (dom/attribute "type" "text")
               (dom/attribute "value" initial-value)
               (dom/attribute "placeholder" (str f " – arg " (dec arg-index)))
               (when list-id (dom/attribute "list" list-id))
               ~(->> (dom/events dom/parent "input")
                     (m/eduction (map dom/target-value))
                     (m/reductions {} initial-value)
                     (m/relieve {})))))

(p/defn page-submissions []
  (dom/div
   (p/binding [hyperfiddle.q4/render-input render-input]
     (hyperfiddle.q4/hfql [{((submissions .) ::hf/render submissions-renderer)
                            [;;:db/id
                             (:dustingetz/email ::hf/render render-string)
                             (:dustingetz/gender ::hf/render render-typeahead ::hf/options (genders) ::hf/as gender)
                             (:dustingetz/shirt-size ::hf/render render-typeahead
                                                     ::hf/options (shirt-sizes gender .))]}]) )))

(p/defn test-node []
  #_~(hf/q '[:find [?a ...] :in $ :where [_ ?a _]] )
  #_(hyperfiddle.q4/hfql [{((submissions "") #_#_::hf/render submissions-renderer) [:db/id
                                                                              (:dustingetz/email #_#_::hf/render render-input)
                                                                              (:dustingetz/gender #_#_::hf/render render-typeahead
                                                                                                  #_#_::hf/options {(genders) [:db/ident]})
                                                                              :dustingetz/shirt-size]}])
  (hyperfiddle.q4/hfql {(genders) [:db/ident]}))

(comment
  (hyperfiddle.rcf/enable!)
  (hyperfiddle.rcf/enable! false)
  (tests
    (p/run (! ~test-node))
    % := _)
  )


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
;; * TODO free variables
;;
;; * TODO +Path-based renderer+
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
;; * TODO Schema-based renderer
;;
;; * TODO Spec extention
;;
