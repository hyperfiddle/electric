(ns dustin.unsequence
  (:require
    [missionary.core :as m :refer [latest relieve watch ap ?!]]
    [minitest #?@(:clj [:refer [tests]])]
    [hyperfiddle.incremental :as I]))

;(defn bind [>x f])
;(defn pure [x])

(defn memoize-n
  "The memoized version of the function keeps a cache of the mapping from the `n`
  first arguments to results."
  [n f]
  (let [mem (atom {})]
    (fn [& args]
      (let [mem-args (take n args)]                         ;; take n args instead of all args
        (if-let [e (find @mem mem-args)]
          (val e)
          (let [ret (apply f args)]
            (swap! mem assoc mem-args ret)
            ret))))))

(def fmap #_[kf f >x]
  (memoize-n 1 (fn [k f >x]
                 (kf a) (latest f >x))))

(defn fmap [f >x] (latest f >x))                            ; (f x) -> meta

(defn capI "test primitive" [Ia] @(Ia #() #()))

(defn extend-seq [kf >xs]                                   ; Incr Coll a -> Incr Coll Incr a
  (fmap
    (fn [xs]
      (vec (for [a xs]
             (pure >xs (kf a) a)
             (pure >xs ^{:key (kf a)} a)
             (fmapK (kf a) (fn [_] a) >xs)
             (fmapK kf (fn [_] a) >xs)
             #_(fmap (fn [_] ^{:key (kf a)} a) >xs))))
    >xs))

(comment
  (def !xs (atom [{:db/id 100 :email "a"}
                  {:db/id 101 :email "b"}
                  {:db/id 102 :email "c"}]))
  (def >xs (m/watch !xs))

  (def >>xs (extend-seq :db/id >xs))
  (def xs> (I/capI >>xs))

  (I/capI (first xs>))

  (force-update xs> {:db/id 100 :email "aaa"})              ; point update
  (I/capI r-100-email) := "aaa"

  (def r-100-email (fmap :email (first xs>)))
  (I/capI r-100-email)

  (I/sequence-some *1)
  (I/capI *1) := [{:db/id 100 :email "a"}
                  {:db/id 101 :email "b"}
                  {:db/id 102 :email "c"}]

  (swap! !xs update-in [0 :email] (constantly "aa"))        ; tree update
  (I/capI r-100-email) := "aa"                              ; fail



  (reset! !xs [{:db/id 100 :email "a"}
               {:db/id 101 :email "b"}
               {:db/id 102 :email "c"}])
  (reset! !xs [{:db/id 100 :email "a"}
               {:db/id 101 :email "b"}])

  (defn render-el [>x]
    [:code (m/latest pr-str x)])

  (defn table [>xs]                                         ; :: Incr Coll x -> Incr Hiccup
    [:div [:h1 "title"]
     [:table
      (let [>>xs (extend-seq :db/id >xs)]
        (reactive-for [>x >>xs]                             ; join
          [:tr (:email >x)]))
      [:span (m/latest count >xs)]]])

  (capI (table >xs)) := [:div _ [:table _]]

  (defn table2 [>xs]                                        ; :: Incr Coll x -> Incr Incr Hiccup
    (join
      (let [>>xs (extend-seq :db/id >xs)]
        [:div [:h1 "title"]
         (->> >>xs
           (fmap (fn [>xs]
                   (->> >xs (fmap (fn [xs]
                                    [:table
                                     (for [x xs]
                                       [:tr (:email x)])
                                     [:span (count xs)]]))))))])))

  (capI (table2 >xs)) := [:div _ [:table _]]


  ; a variable coll of items but each item is constant
  ; a tree result from datomic and that is contstant

  )



(defn render-edn [>x]
  (println 'effect)
  (fmap pr-str >x))

(bind (extend-seq :db/id >xs)
  (fn [xs>]                                                 ; :: Coll Incr a , e.g. [>a >b >c]
    (for [>x xs>]
      (render-edn >x))))