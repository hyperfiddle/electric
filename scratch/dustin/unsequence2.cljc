(ns dustin.unsequence2
  (:require
    [dustin.fiddle :refer [submissions]]
    [missionary.core :as m :refer [latest relieve watch ap ?!]]
    [minitest #?@(:clj [:refer [tests]])]
    [hyperfiddle.incremental :as I :refer [joinI fmapI capI bindI]]))


(defn extend-seq [kf >xs]                                   ; Incr Coll a -> Incr Coll Incr a
  (fmapI
    (fn [xs]
      (vec (for [a xs]
             (fmapI (fn [_]
                      ; todo memoizing-fmap
                      ^{:key (kf a)} a)
               >xs))))
    >xs))

; Monad m => join :: m (m a) -> m a
; Comonad m => extend :: m a -> m (m a)

(tests
  (def >>xs (extend-seq :db/id (m/watch (atom [{:db/id 1}
                                               {:db/id 2}
                                               {:db/id 3}]))))
  (capI >>xs)
  ;:= '[_ _ _]

  (capI (bindI >>xs I/sequence-some))
  := [#:db{:id 1} #:db{:id 2} #:db{:id 3}]

  (capI (I/sequence-some (capI >>xs)))
  := [#:db{:id 1} #:db{:id 2} #:db{:id 3}]
  )

(defn email [x]
  (println 'email)                                          ; shows lack of memoizing
  (:email x))

(defn table [>xs]
  ;(joinI)
  ; can't join here, hiccup is interleaved with flows
  (let [>>xs (extend-seq :db/id >xs)]
    (->> >>xs
      (fmapI (fn [xs>]
               (println 'xs> xs>)
               [:div [:h1 "title"]
                [:table
                 [:<>
                  [:span (count xs>)] #_(->> >xs (fmapI (fn [xs] [:span (count xs)])))
                  (vec (for [>x xs>]
                         [:tr (fmapI email >x)]))]]])))))

(tests
  (def !xs (atom nil))
  (def >xs (m/watch !xs))
  (def >table (table >xs))
  ;>table := '_
  (def !table-view (>table #(prn :ready) #()))

  (reset! !xs #_(submissions "")
    [{:db/id 100 :email "a"}
     {:db/id 101 :email "b"}
     {:db/id 102 :email "c"}])
  ;@!table-view := '[:div [:h1 "title"] [:table [:<> _]]]

  ;(def tree *1)
  ;(capI (get-in tree [2 1 2 0 1]))

  (I/capI (I/sequence-some @!table-view))
  := [:div [:h1 "title"]
      [:table [:<> [:span 3] [[:tr "a"] [:tr "b"] [:tr "c"]]]]]

  (reset! !xs #_(submissions "alice")
    [{:db/id 100 :email "a"}
     {:db/id 101 :email "b"}
     #_{:db/id 102 :email "c"}])
  (I/capI (I/sequence-some @!table-view))
  :=  [:div [:h1 "title"] [:table [:<> [:span 2] [[:tr "a"] [:tr "b"]]]]]

  )

; next steps
; implement extend-seq against missionary correctly (hard), Leo will think
; possibly continue with a bad impl
; trace this database/table dag (`table` in different reactor than`submission`)