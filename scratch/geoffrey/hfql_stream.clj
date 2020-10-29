(ns geoffrey.hfql-stream
  (:require [clojure.walk :as walk]
            [dustin.fiddle :refer :all]
            [dustin.hf-nav :refer :all]
            [dustin.monad-scope :refer [runScope pure bind fmap sequence]]
            [hyperfiddle.fabric :as df]
            [meander.epsilon :as m :refer [match]]
            [minitest :refer [tests]]))

(defn trace [message v] #_(println message " => " v) v)

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(defn hf-apply [edge a scope]
  (cond
    (keyword? edge) (df/fmap #(hf-nav edge %) a)
    (= '* edge)     (df/fmap touch1 a)
    (seq? edge)     (let [[f & args] edge]

                      (apply df/fmap @(clojure.core/resolve f) (replace scope args)))
    ()              (println "hf-eval unmatched edge: " edge)))

(defn hf-eval [edge a]
  (fn [scope]
    (trace "hf-eval" [edge a])
    (let [b (hf-apply edge a scope)]
      [{(hf-edge->sym edge) b, '% b}
       b])))

(defn hf-pull' [pat ma]
  (match pat
    [!pats ...]
    (->> !pats
         (mapv (fn [pat] (hf-pull' pat ma)))
         (sequence)
         (fmap (fn [>as] (apply df/fmap (fn [& args]
                                         (trace "merge" args)
                                         (apply merge args)) >as))))                             ; parallel descent (same env) ;; TODO optimize

    {& (m/seqable [?edge ?pat])}
    (as-> ma ma
      (bind ma (partial hf-eval ?edge))
      (hf-pull' ?pat ma)                                    ; recursive bind
      (fmap (fn [>a] (df/fmap (fn [a] (trace "?edge" {?edge a})) >a)) ma))

    ?leaf
    (as-> ma ma
      (bind ma (partial hf-eval ?leaf))
      (fmap (fn [>a] (df/fmap (fn [a] (trace "?leaf" {?leaf a})) >a)) ma))
    ))


(defn hf-pull! [pat v scope]
  (let [[_ v] (runScope (hf-pull' pat (pure v)) scope)]
    v))

(defmacro hf-pull [pat v & [scope]]
  `(hf-pull! (quote ~pat) ~v #_(quote) ~(or scope {})))



(defmacro with-input [value query]
  `(let [result#  (atom nil)
         ~'input (df/input)
         output#  ~query]
     (df/on output# (partial reset! result#))
     (df/put ~'input ~value)
     @result#))

(tests

 (with-input :dustingetz/male
   (hf-pull (identity %) :dustingetz/male {'% input}))
 => '{(identity %) :dustingetz/male}

 (with-input 17592186045429
   (hf-pull :dustingetz/gender input))
 => '#:dustingetz{:gender :dustingetz/male}

 (with-input 17592186045429
   (hf-pull [{:dustingetz/gender [:db/ident]}] input))
 => '#:dustingetz{:gender #:db{:ident :dustingetz/male}}


 (with-input dustin.dev/*$*
   (hf-pull (gender $) nil {'$ input}))
 => '{(gender $) 17592186045418}

 (with-input "alic"
   (hf-pull (submission needle) nil {'needle input}))
 => '{(submission needle) 17592186045428}

 (with-input "alic"
   (hf-pull {(submission needle) [:dustingetz/gender]}  nil {'needle input}))
 => '{(submission needle) #:dustingetz{:gender :dustingetz/female}}

 (with-input "alic"
   (hf-pull {(submission needle) [:dustingetz/gender]} nil {'needle input}))
 => '{(submission needle) #:dustingetz{:gender :dustingetz/female}}

 (with-input "alic"
   (hf-pull {(submission needle) [{:dustingetz/gender [:db/ident]}]} nil {'needle input}))
 => '{(submission needle) #:dustingetz{:gender #:db{:ident :dustingetz/female}}}

 (with-input "alic"
   (hf-pull {(submission needle) {:dustingetz/gender (shirt-size dustingetz/gender)}} nil {'needle input}))
 => '{(submission needle) #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045425}}}

 (with-input "alic"
   (hf-pull {(submission needle) [{:dustingetz/gender [{(shirt-size dustingetz/gender) [:db/id]}]}]} nil {'needle input}))
 => '{(submission needle) #:dustingetz{:gender {(shirt-size dustingetz/gender) {:db/id 17592186045425}}}}

 (with-input "alic"
   (hf-pull
    {(submission needle) ; query
     [{:dustingetz/gender
       [{(shirt-size dustingetz/gender)
         [:db/ident]}]}]}
    nil
    {'needle input}))                                      ; scope

 => '{(submission needle) ; result
      {:dustingetz/gender
       {(shirt-size dustingetz/gender)
        {:db/ident :dustingetz/womens-medium}}}}

 (with-input 17592186045418
   (hf-pull [{:db/ident [:db/id]}] input))
 => #:db{:ident #:db{:id 17592186045418}}

 (with-input 17592186045421
   (hf-pull [{:dustingetz/gender [{:db/ident [:db/id]}]}] input))
 => #:dustingetz{:gender #:db{:ident #:db{:id 17592186045418}}}

 (with-input 17592186045421
   (hf-pull [{:dustingetz/gender [{:db/ident [{:db/ident [:db/ident]}]}]}] input))
 => #:dustingetz{:gender #:db{:ident #:db{:ident #:db{:ident :dustingetz/male}}}}

 (with-input 17592186045421
   (hf-pull [{:dustingetz/gender [:db/id]}] input))
 => #:dustingetz{:gender #:db{:id 17592186045418}}

 (with-input 17592186045421
   (hf-pull [{:dustingetz/gender [{:db/id [:db/id]}]}] input))
 => #:dustingetz{:gender #:db{:id #:db{:id 17592186045418}}}

; :db/id is a self reference so this actually is coherent
 (with-input 17592186045421
   (hf-pull [{:dustingetz/gender [{:db/id [{:db/id [{:db/id [:db/id]}]}]}]}] input))
 => #:dustingetz{:gender #:db{:id #:db{:id #:db{:id #:db{:id 17592186045418}}}}}

 )
