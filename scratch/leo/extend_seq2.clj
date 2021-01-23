(ns leo.extend-seq2
  (:require [missionary.core :as m]
            [minitest :refer [tests]]
            [clojure.set :as set]))


(declare extend-seq fmap)


(defmulti render (fn [a] (type a)))
(defmethod render ::set [])
(defmethod render ::map [])
(defmethod render ::string [])

; Need tracing example with dynamic creation of new nodes
; p-q-cross doesn't create new nodes, it just chooses between existing nodes



; UI glues together the tracing with extend-seq

(defn table [>tree]                                         ; :: Incr Incr Incr ...
  (let [>submissions (fmap #(get-in % ['(submissions needle)]) >tree)
        >submissions (extend-seq :db/id >submissions)]
    (->> >submissions
      (fmap (fn [xs>]
              [:table
               [:span (count xs>)]
               (vec (for [>a xs>]
                      [:tr
                       [:td (fmap :email >a)]
                       [:td (->> >a
                              (fmap (fn [a]
                                      (get-in a [:gender '(shirt-sizes gender)])))
                              (fmap (fn table-shirt-sizes [>shirt-sizes]
                                      [:table
                                       [:span (fmap count >shirt-sizes)]]
                                      )))]]))])))))

(defn App [>xs]
  [:div [:h1 "title"]
   (table >xs)])

(tests
  (def !results (atom nil))

  (def result>>>> (hfql
                    [{(submissions needle)
                      [{:dustingetz/gender
                        [{(shirt-sizes gender)
                          [:db/id :db/ident]}]}]}]))

  (reset! !results '{(submissions needle) [#:dustingetz{:email  "alice@example.com",
                                                        :gender {:db/ident            :dustingetz/female,
                                                                 (shirt-sizes gender) [#:db{:ident :dustingetz/womens-small}
                                                                                       #:db{:ident :dustingetz/womens-medium}
                                                                                       #:db{:ident :dustingetz/womens-large}]}}
                                           #:dustingetz{:email  "bob@example.com",
                                                        :gender {:db/ident            :dustingetz/male,
                                                                 (shirt-sizes gender) [#:db{:ident :dustingetz/mens-small}
                                                                                       #:db{:ident :dustingetz/mens-medium}
                                                                                       #:db{:ident :dustingetz/mens-large}]}}
                                           #:dustingetz{:email  "charlie@example.com",
                                                        :gender {:db/ident            :dustingetz/male,
                                                                 (shirt-sizes gender) [#:db{:ident :dustingetz/mens-small}
                                                                                       #:db{:ident :dustingetz/mens-medium}
                                                                                       #:db{:ident :dustingetz/mens-large}]}}],
                     (genders)            [#:db{:ident :dustingetz/male} #:db{:ident :dustingetz/female}]}
    )

  (def >results (m/watch !results))
  (capI (join (table >results)))
  := [:div [:h1 "title"] [:table [:<> [:span 2] [[:tr "a"] [:tr "b"]]]]]


  (reset! !result '#{#:dustingetz{:email  "alice@example.com",
                                  :gender {:db/ident                       :dustingetz/female,
                                           (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/womens-small}
                                                                            #:db{:ident :dustingetz/womens-medium}
                                                                            #:db{:ident :dustingetz/womens-large}]}}
                     #:dustingetz{:email  "bob@example.com",
                                  :gender {:db/ident                       :dustingetz/male,
                                           (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                            #:db{:ident :dustingetz/mens-medium}
                                                                            #:db{:ident :dustingetz/mens-large}]}}
                     #:dustingetz{:email  "charlie@example.com",
                                  :gender {:db/ident                       :dustingetz/male,
                                           (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                            #:db{:ident :dustingetz/mens-medium}
                                                                            #:db{:ident :dustingetz/mens-large}]}}})


  (def nested-item (->> (extend-seq :dustingetz/email (m/watch tree))
                     (comment TODO)))

  (def !nested-item (nested-item #(prn :ready) #(prn :done)))

  (reset! tree (comment TODO update nested item))

  @!nested-item := (comment updated-value)

  )