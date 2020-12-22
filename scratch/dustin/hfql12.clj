(ns dustin.hfql12
  "fmap"
  (:refer-clojure :exclude [eval])
  (:require
    [clojure.walk :refer [walk prewalk postwalk]]
    [contrib.do :refer [via* Do-via *this !]]
    [datomic.api :as d]
    [dustin.fiddle :refer :all]
    [dustin.hf-nav :refer :all]
    [dustin.monad-rw :refer [runRW pure bind fmap]]
    [hyperfiddle.api :as hf]
    [meander.epsilon :as m :refer [match rewrite]]
    [minitest :refer [tests]]))


(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(defn hf-apply [edge a scope]
  (cond
    (keyword? edge) (hf-nav edge a #_(get scope '%))
    (seq? edge) (let [[f & args] edge]
                  (clojure.core/apply (clojure.core/resolve f) (replace scope args)))
    () (println "hf-eval unmatched edge: " edge)))

(defn hf-eval [edge a]                                      ; name?
  (fn [scope]
    (let [b (hf-apply edge a scope)]
      [{(hf-edge->sym edge) b, '% b}
       b])))

(defn hf-pull [pat ma]
  (match pat

    {& (m/seqable [?edge ?pat])}
    (as-> ma ma
      (bind ma (partial hf-eval ?edge))
      (hf-pull ?pat ma)                                     ; recursive bind
      ;(discardW ma)                                         ; unwind the scope, is this the right way?
      (fmap (fn [a] {?edge a}) ma))

    ?leaf
    (as-> ma ma
      (bind ma (partial hf-eval ?leaf))
      (fmap (fn [a] {?leaf a}) ma))
    ))

(defn hf-pull! [pat v scope]
  (let [[_ v] (runRW (hf-pull pat (pure v)) scope)]
    v))

(tests

  (def ma (pure :dustingetz/male))
  (runRW ma {})
  (runRW (hf-pull :db/ident ma) {})
  := '[{db/ident :dustingetz/male, % :dustingetz/male} #:db{:ident :dustingetz/male}]
  ; why do we see the ident in scope here

  ; Tests use hf-pull! runner to test what matters, the result, as I de-bug the scope monad

  (hf-pull! '(hf-nav :db/ident %) :dustingetz/male {'% :dustingetz/male})
  := {'(hf-nav :db/ident %) :dustingetz/male}

  (hf-pull! '(identity %) :dustingetz/male {'% :dustingetz/male})
  := '{(identity %) :dustingetz/male}

  (hf-pull! :dustingetz/gender 17592186045441 {})
  := '#:dustingetz{:gender :dustingetz/male}

  ;(hf-pull '(:dustingetz/gender %) {'% 17592186045441})
  ;:= {(:dustingetz/gender %) (:dustingetz/gender 17592186045441)} ; ClassCastException

  (hf-pull! {:dustingetz/gender :db/ident} 17592186045441 {})
  := ' #:dustingetz{:gender #:db{:ident :dustingetz/male}}

  (hf-pull! '(gender) nil {})
  := '{(gender) 17592186045430}

  (hf-pull! '(submission needle) nil {'needle "alic"})
  := '{(submission needle) 17592186045440}

  (hf-pull! {'(submission needle) :dustingetz/gender}  nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender :dustingetz/female}}

  (hf-pull! {'(submission needle) :dustingetz/gender} nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender :dustingetz/female}}

  (hf-pull! {'(submission needle) {:dustingetz/gender :db/ident}} nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender #:db{:ident :dustingetz/female}}}

  (hf-pull! '{(submission needle) {:dustingetz/gender (shirt-size dustingetz/gender)}} nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045436}}}

  #_(runRW (hf-pull '{(submission needle) {:dustingetz/gender (shirt-size dustingetz/gender)}} (pure nil)) {'needle "alic"})
  (hf-pull! '{(submission needle) {:dustingetz/gender (shirt-size dustingetz/gender)}} nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045436}}}

  (hf-pull!
    '{(submission needle)                                   ; query
      {:dustingetz/gender
       {(shirt-size dustingetz/gender)
        :db/ident}}}
    nil
    {'needle "alic"})                                       ; scope

  := '{(submission needle)                                  ; result
       {:dustingetz/gender
        {(shirt-size dustingetz/gender)
         {:db/ident :dustingetz/womens-small}}}}

  (hf-pull! {:db/ident :db/id} 17592186045430 {})
  := #:db{:ident #:db{:id 17592186045430}}

  (hf-pull! {:dustingetz/gender {:db/ident :db/id}} 17592186045441 {})
  := #:dustingetz{:gender #:db{:ident #:db{:id 17592186045430}}}

  (hf-pull! {:dustingetz/gender {:db/ident {:db/ident :db/ident}}} 17592186045441 {})
  := #:dustingetz{:gender #:db{:ident #:db{:ident #:db{:ident :dustingetz/male}}}}

  (hf-pull! {:dustingetz/gender :db/id} 17592186045441 {})
  := #:dustingetz{:gender #:db{:id 17592186045430}}

  (hf-pull! {:dustingetz/gender {:db/id :db/id}} 17592186045441 {})
  := #:dustingetz{:gender #:db{:id #:db{:id 17592186045430}}}

  ; :db/id is a self reference so this actually is coherent
  (hf-pull! {:dustingetz/gender {:db/id {:db/id {:db/id :db/id}}}} 17592186045441 {})
  := #:dustingetz{:gender #:db{:id #:db{:id #:db{:id #:db{:id 17592186045430}}}}}

  )