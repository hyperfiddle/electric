(ns user.crud4
  (:require [datascript.core :as d]
            [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode node vars main]]
            [missionary.core :as m]
            [geoffrey.fiddle-effects
             :refer [submissions genders shirt-sizes submission-details
                     submission gender shirt-size]]
            [hyperfiddle.q2 :refer [hf-nav hfql]]))

; ui widgets are server biased due to needing db parameter
(defnode hello-world-widgets [db e]
  (let [v (d/touch (d/entity db e))]
    ~@[:pre (js/pprint-str v)]))

(tests
  #?(:clj (def !db (atom (d/db *conn*))))
  #?(:cljs (def !e (atom [:dustingetz/email "alice@example.com"])))

  ; entrypoint is client
  (def sampler1 (test-dag (let [e ~(m/watch !e)] ~@(hello-world ~(m/watch !db) e))))
  (def sampler2 (test-dag ~@(hello-world ~(m/watch !db) ~@~(m/watch !e))))
  ; double transfer is equivalent
  [@sampler1 @sampelr2] := [?x ?x])

;(defnode select-widget [& {:keys [:on-change]}]
;  [:select {:on-change (fn [])}])

(defnode select-widget [& {:keys [::ref ::value ::on-change
                                  ::options ::render-option]}]
  (do
    (let [!temp (atom nil)
          temp ~(m/watch !temp)]
      ~@[:select {:value temp :on-change (fn [v] (reset! !temp v))}
         [[:option]
          [:option]]])
    ...)



  ~@(let [!needle (atom "")
          needle ~(m/watch !needle)
          ;x ~(m/watch ref)

          ]
      [:<> #_[:input.select-needle {:value needle :on-change #(reset! !needle %)}]
       (select-widget {:selected value :on-change on-change
                       #_(node [] (let [v (.. e -target -value)] ~@(reset! ref v)))}
                      ~@(for [x (options #_~@needle)]
                          ~@(render-option x)))]))

(defnode email-taken [db email]
  (some? (d/q '[:in ?x $ :find ?e . :where [?e :dustingetz/email ?x]] db email)))

(defnode shirt-size [db e]
  (let [!e (d/entity db e)]
    (select-widget
      ;::ref (atom (:dustingetz/shirt-size !e))
      ::value (:dustingetz/shirt-size !e)
      ::options (node [needle]
                  (d/q '[:in $ ?gender :find [?e ...] :where
                         [?e :dustingetz/type :dustingetz/shirt-size]
                         [?e :dustingetz/gender ?gender]]
                       db (:dustingetz/gender !e) (or needle "")))
      ::render-option (node [option] [:option (name ~@(:db/ident (d/entity db option)))]))))

(tests
  #?(:clj (def !db (atom (d/db *conn*))))
  #?(:cljs (def !e (atom [:dustingetz/email "alice@example.com"])))

  (def sampler (test-dag ~@(shirt-size ~(m/watch !db) ~@~(m/watch !e))))
  @sampler := _)
