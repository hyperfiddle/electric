(ns dustin.trace30
  (:require
    [minitest :refer [tests]]
    [hyperfiddle.incremental :as I
     :refer [sequenceI sequence-mapI
             bindI pureI fmapI capI joinI incr?]]
    [hyperfiddle.hfql20 :refer [hfql]]
    [leo.extend-seq :refer [extend-seq]]
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]))


(tests
  1 := 1)

(let [needle (pureI "")]
  (hfql [{(submissions needle)
          [{:dustingetz/gender
            [{(shirt-size gender)
              [:db/id :db/ident]}]}]}]))
(capI (I/sequence-some *1))

(let [needle (pureI "")] (hfql (submissions needle)))

(macroexpand-1 '(hfql [{(submissions needle) [:dustingetz/email :db/id]}]))

(tests
  (let [needle (pureI "")] (hfql [{(submissions needle) [:dustingetz/email :db/id]}]))
  ;(let [needle (pureI "")] (hfql [{(submission needle) [:dustingetz/email :db/id]}]))

  (capI (I/sequence-some *1))
  ;:= '{(submissions needle) _}
  := '{(submissions needle) [{:dustingetz/email "alice@example.com", :db/id 9}
                             {:dustingetz/email "bob@example.com", :db/id 10}
                             {:dustingetz/email "charlie@example.com", :db/id 11}]}
  ; there are arbitrarily nested layers and we don't want all of them immediately

  (def x *1)


  (capI (I/sequence-some x))
  := '{(submissions needle) [#:dustingetz{:db/id _ :email _}
                             #:dustingetz{:db/id _ :email _}
                             #:dustingetz{:db/id _ :email _}]}

  #_(capI (-> x I/sequence-some (bindI I/sequence-some)))
  #_(capI (joinI (fmapI I/sequence-some (I/sequence-some x))))
  (capI (->> x I/sequence-some (fmapI I/sequence-some) joinI))
  := '{(submissions needle) [{:dustingetz/email "alice@example.com", :db/id 9}
                             {:dustingetz/email "bob@example.com", :db/id 10}
                             {:dustingetz/email "charlie@example.com", :db/id 11}]}
  )

(defn table-view [tree>]
  (let [a 42
        >submissions (get-in tree> ['(submissions needle)])]
    [:table
     [:span (inc (count ~>submissions))]
     #_[:pre (I/sequence-some >submissions)]
     (->> (extend-seq :db/id >submissions)
       (fmapI (fn [xs>]
                (for [>x xs>]
                  (fmapI vector :tr (fmapI pr-str a >x))
                  #_[:tr (pr-str x)]))))]))


(defn table-view-user [tree>]
  (let [>submissions (get-in tree> ['(submissions needle)])
        y (count ~>submissions)]
    [:table
     [:span y]
     (for :db/id [>x ~>submissions]
                 [:tr (pr-str ~>x)])]))

(tests
  "all together"
  (table-view
    (let [needle (pureI "")]
      (hfql [{(submissions needle) [:dustingetz/email :db/id]}])))
  (capI *1)
  := [:table [:span 3]]
  )


(test


  (macroexpand-1 '(hfql [{(submissions needle) [:dustingetz/email :db/id]}]))
  := '{(quote (submissions needle))
       (bindI (hyperfiddle.incremental/extend-seq (fmapI submissions needle))
         (fn [>as]
           (vec
             (for [% >as]
               (let [% %]
                 {(quote :dustingetz/email) (fmapI (partial hyperfiddle.hfql20/hf-nav :dustingetz/email) %),
                  (quote :db/id)            (fmapI (partial hyperfiddle.hfql20/hf-nav :db/id) %)})))))}

  (def !needle (atom ""))
  (def >needle (m/watch !needle))

  (def ast (let [visit #?(:cljs table-view :clj identity)]
             ;`(~visit); this is very wrong. all peers see all the AST. server sees table-view nodes, just ignores them
             `{(quote (submissions >needle))
               (I/sequence-some
                 (bindI NODE1 (fn [>as]
                                (vec
                                  (for [% >as]
                                    (let [% %]
                                      {(quote :dustingetz/email) (fmapI (partial hyperfiddle.hfql20/hf-nav :dustingetz/email) %),
                                       (quote :db/id)            (fmapI (partial hyperfiddle.hfql20/hf-nav :db/id) %)}))))
                   (hyperfiddle.incremental/extend-seq (fmapI submissions >needle))))}
             ))

  (def r (trace/reactor! {'>needle !needle} (compile ast)))
  (def !trace (trace/log! r))
  @!trace := [{
               [0]    [#{9 10 11} #{}]
               [0 9]  {:dustingetz/email "alice@example.com", :db/id 9}
               [0 10] {:dustingetz/email "bob@example.com", :db/id 10}
               [0 11] {:dustingetz/email "charlie@example.com", :db/id 11}
               ;1      '{(submissions needle) [{:dustingetz/email "alice@example.com", :db/id 9}
               ;                               {:dustingetz/email "bob@example.com", :db/id 10}
               ;                               {:dustingetz/email "charlie@example.com", :db/id 11}]}
               }]

  @((get reactor %) #() #())
  := '{(submissions needle) [{:dustingetz/email "alice@example.com", :db/id 9}
                             {:dustingetz/email "bob@example.com", :db/id 10}
                             {:dustingetz/email "charlie@example.com", :db/id 11}]}


  (reset! !needle "bob")
  ;keypath encodes which bind (place in the AST), and dynamic values eval'ed
  @!trace := [{
               [0]    [#{9 10 11} #{}]
               [0 9]  {:dustingetz/email "alice@example.com", :db/id 9}
               [0 10] {:dustingetz/email "bob@example.com", :db/id 10}
               [0 11] {:dustingetz/email "charlie@example.com", :db/id 11}}
              {[0] [#{} #{9 11}]}]

  (def r-client (trace/reactor!
                  ; list passive nodes
                  {NODE1 ...}
                  (compile ast)))
  (def !trace (trace/log! r))

  ;(render-table >submissions) := [:table [:span 3]]
  (def >submissions (get client-r [0]))                     ; this is a source map
  (render-table >submissions) := [:table [:span 3]]
  )


(defn render-edn [>tree] [:pre (pr-str ~>tree)])
; The trace lets us do better than this ^




;(replay! ...)


