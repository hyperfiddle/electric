(ns dustin.hfql14
  "dataflow"
  (:refer-clojure :exclude [eval])
  (:require
    [clojure.walk :refer [walk prewalk postwalk]]
    [dustin.fiddle :refer :all]
    [dustin.hf-nav :refer :all]
    [dustin.monad-scope :refer [runScope pure bind fmap]]
    [meander.epsilon :as m :refer [match rewrite]]
    [minitest :refer [tests]]))


; HFQL monad :: Scope Stream a

(defn pure [>a]
  (fn [scope]                                               ; reader
    [scope                                                  ; stack state
     >a]))                                                   ; stream

; does the scope have streams in it, yes it does

(defn fmap' [f ma]
  (fn [scope]
    (let [[s >a] (runScope ma scope)]
      [s >a])))

(defn runScope [m scope] (m scope))

(defn bind [ma mf]
  (fn [scope]
    (let [[log-a a] (runScope ma scope)
          scope' (merge scope log-a {'% a})]                ; accumulate new vars that shadow old vars
      (runScope (mf a) scope'))))                              ; pass scope forward only (into the continuation)

(defn fmap [f ma]                                           ; mv :: scope -> a; f :: a -> b
  (fn [scope]                                               ; *this
    (let [[scope' a] (ma scope)
          b (f a)]                                          ; fmap neither reads nor writes
      [(assoc scope' '% b)                                  ; auto-set % ?
       b])))

(tests

  (def >a (input))
  (runScope (pure >a) {'>b (input)})
  := [{'>b ...} >a]
  (put >a 42)


  (runScope (pure 42) {})
  := [{} 42]

  (def ma (pure 42))
  (runScope (bind ma pure) {})
  := [{} 42]

  (runScope (bind ma (fn [a] (fn [scope] [{'a a} (inc a)]))) {})
  := '[{a 42} 43]

  (runScope (bind ma (fn [a] (fn [scope] [scope (inc (get scope 'z))]))) {'z 0})
  := '[{z 0} 1]

  (runScope (bind ma (fn [a] (fn [scope] [{'z 1} (inc (get scope 'z))]))) {'z 0})
  := '[{z 1} 1]

  (runScope (fmap inc ma) {'x 99})
  := [{'x 99} 43]

  (runScope (fmap inc (fmap inc ma)) {'x 99})
  := [{'x 99} 44]
  )



; HFQL driver

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(defn hf-apply [edge a scope]                               ; (! :Scope.ask arg)
  (cond
    (keyword? edge) (hf-nav edge a #_(get scope '%))
    (seq? edge) (let [[f & args] edge]
                  (clojure.core/apply (clojure.core/resolve f) (replace scope args)))
    () (println "hf-eval unmatched edge: " edge)))

(defn hf-eval [edge a]                                      ; name?
  (fn [scope]
    (let [b (hf-apply edge a scope)]
      [{(hf-edge->sym edge) b}                              ; (! :Scope.tell edge b)
       b])))

(defn hf-pull' [pat >ma]
  (match pat

    {& (m/seqable [?edge ?pat])}
    (as-> >ma >ma
      (bind >ma (partial hf-eval ?edge))
      (hf-pull' ?pat >ma)                                    ; recursive bind
      (fmap (fn [a] {?edge a}) >ma))

    ?leaf
    (as-> ma ma
      (bind ma (partial hf-eval ?leaf))
      (fmap (fn [a] {?leaf a}) ma))
    ))

(defn hf-pull! [pat v scope]
  (let [[_ v] (runScope (hf-pull' pat (pure v)) scope)]
    v))

(defmacro hf-pull [pat v & [scope]]
  `(hf-pull! (quote ~pat) ~v #_(quote) ~(or scope {})))

(tests

  (def m (pure :dustingetz/male))
  (runScope m {})
  := [{} :dustingetz/male]
  (runScope (hf-pull' :db/ident m) {})
  := '[{db/ident :dustingetz/male, % :dustingetz/male} #:db{:ident :dustingetz/male}]
  ; why do we see the ident in scope here

  ; Tests use hf-pull! runner to test what matters, the result, as I de-bug the scope monad

  (macroexpand '(hf-pull (hf-nav :db/ident %) :dustingetz/male {% :dustingetz/male}))
  (hf-pull (hf-nav :db/ident %) :dustingetz/male {'% :dustingetz/male})
  := {'(hf-nav :db/ident %) :dustingetz/male}

  (hf-pull (identity %) :dustingetz/male {'% :dustingetz/male})
  := '{(identity %) :dustingetz/male}

  (hf-pull :dustingetz/gender 17592186045441)
  := '#:dustingetz{:gender :dustingetz/male}

  ;(hf-pull '(:dustingetz/gender %) {'% 17592186045441})
  ;:= {(:dustingetz/gender %) (:dustingetz/gender 17592186045441)} ; ClassCastException

  (hf-pull {:dustingetz/gender :db/ident} 17592186045441)
  := '#:dustingetz{:gender #:db{:ident :dustingetz/male}}


  (hf-pull (gender) nil)
  := '{(gender) 17592186045430}

  (hf-pull (submission needle) nil {'needle "alic"})
  := '{(submission needle) 17592186045440}

  (hf-pull {(submission needle) :dustingetz/gender}  nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender :dustingetz/female}}

  (hf-pull {(submission needle) :dustingetz/gender} nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender :dustingetz/female}}

  (hf-pull {(submission needle) {:dustingetz/gender :db/ident}} nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender #:db{:ident :dustingetz/female}}}

  (hf-pull {(submission needle) {:dustingetz/gender (shirt-size dustingetz/gender)}} nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045436}}}

  #_(runScope (hf-pull '{(submission needle) {:dustingetz/gender (shirt-size dustingetz/gender)}} (pure nil)) {'needle "alic"})
  (hf-pull {(submission needle) {:dustingetz/gender (shirt-size dustingetz/gender)}} nil {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045436}}}

  (hf-pull
    {(submission needle)                                   ; query
     {:dustingetz/gender
      {(shirt-size dustingetz/gender)
       :db/ident}}}
    nil
    {'needle "alic"})                                       ; scope

  := '{(submission needle)                                  ; result
       {:dustingetz/gender
        {(shirt-size dustingetz/gender)
         {:db/ident :dustingetz/womens-small}}}}

  (hf-pull {:db/ident :db/id} 17592186045430)
  := #:db{:ident #:db{:id 17592186045430}}

  (hf-pull {:dustingetz/gender {:db/ident :db/id}} 17592186045441)
  := #:dustingetz{:gender #:db{:ident #:db{:id 17592186045430}}}

  (hf-pull {:dustingetz/gender {:db/ident {:db/ident :db/ident}}} 17592186045441)
  := #:dustingetz{:gender #:db{:ident #:db{:ident #:db{:ident :dustingetz/male}}}}

  (hf-pull {:dustingetz/gender :db/id} 17592186045441)
  := #:dustingetz{:gender #:db{:id 17592186045430}}

  (hf-pull {:dustingetz/gender {:db/id :db/id}} 17592186045441)
  := #:dustingetz{:gender #:db{:id #:db{:id 17592186045430}}}

  ; :db/id is a self reference so this actually is coherent
  (hf-pull {:dustingetz/gender {:db/id {:db/id {:db/id :db/id}}}} 17592186045441)
  := #:dustingetz{:gender #:db{:id #:db{:id #:db{:id #:db{:id 17592186045430}}}}}

  )





'{(my-query)
  [:db/id
   :person/name
   {:person/gender
    {(shirt-size dustingetz/gender)
     {:db/ident :dustingetz/womens-small}}}]}
