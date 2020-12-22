(ns dustin.hfql9
  "hf-pull with sexp"
  (:require
    [clojure.walk :refer [walk prewalk postwalk]]
    [contrib.do :refer [via* Do-via *this !]]
    [datomic.api :as d]
    [dustin.fiddle :refer :all]
    [dustin.hf-nav :refer :all]
    [hyperfiddle.api :as hf]
    [meander.epsilon :as m :refer [match rewrite]]
    [minitest :refer [tests]]))


(defn apply-scope [sexp scope]                              ; interpret keywords properly?
  #_(println 'apply-scope "sexp: " sexp "scope: " scope)
  (let [[f & asks] sexp
        args (replace scope asks)]
    #_(cons f args)
    #_(println 'apply-scope "f: " f "args: " args)
    (clojure.core/apply (clojure.core/resolve f) args)))

(tests
  ;(apply-scope '(:dustingetz/gender %) {'% 17592186045441}) := ClassCastException
  (apply-scope '(hf-nav :db/ident %) {'% :dustingetz/male}) := :dustingetz/male
  (:dustingetz/gender 17592186045441) := nil                ; fyi
  )

(defn hf-apply [edge scope]
  #_(println 'hf-apply "edge: " edge "scope: " scope)
  (cond
    (keyword? edge) (hf-nav edge (get scope '%))
    (seq? edge) (apply-scope edge scope)
    () (println "hf-apply unmatched edge: " edge)))

(defn hf-pull [pat scope]
  (match pat

    {& (m/seqable [?edge ?pat])}                            ; one entry
    (let [v (hf-apply ?edge scope)
          edge (if (keyword? ?edge) (symbol ?edge) ?edge)
          scope (merge scope {edge v '% v})]
      {?edge (hf-pull ?pat scope)})

    ?edge                                                   ; :dustingetz/gender '(f dustingetz/gender)
    {?edge (hf-apply ?edge scope)}

    ;?_ (doto ?_ (println 'unmatched))
    ))

(tests

  (hf-pull :db/ident {'% :dustingetz/male})
  := #:db{:ident :dustingetz/male}

  ;(hf-pull '(:db/ident %) {'% :dustingetz/male})            ; interpret kw as entity nav? There's no need to, don't do this
  ;:= {(:db/ident %) (:db/ident :dustingetz/male)} ; ClassCastException

  (hf-pull '(hf-nav :db/ident %) {'% :dustingetz/male})
  := {'(hf-nav :db/ident %) :dustingetz/male}

  (hf-pull '(identity %) {'% :dustingetz/male})
  := {'(identity %) :dustingetz/male}

  (hf-pull :dustingetz/gender {'% 17592186045441})
  := #:dustingetz{:gender :dustingetz/male}

  ;(hf-pull '(:dustingetz/gender %) {'% 17592186045441})
  ;:= {(:dustingetz/gender %) (:dustingetz/gender 17592186045441)}

  (hf-pull {:dustingetz/gender :db/ident} {'% 17592186045441})
  := #:dustingetz{:gender #:db{:ident :dustingetz/male}}

  (hf-pull '(gender) {})
  := '{(gender) 17592186045430}

  (hf-pull '(submission needle) {'needle "alic"})
  := '{(submission needle) 17592186045440}

  (hf-pull {'(submission needle) :dustingetz/gender} {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender :dustingetz/female}}

  (hf-pull {'(submission needle) {:dustingetz/gender :db/ident}} {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender #:db{:ident :dustingetz/female}}}

  (hf-pull '{(submission needle) {:dustingetz/gender (shirt-size dustingetz/gender)}} {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045436}}}

  (hf-pull '{(submission needle) {:dustingetz/gender {(shirt-size dustingetz/gender) :db/ident}}} {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/womens-small}}}}

  (hf-pull {:db/ident :db/id} {'% 17592186045430})
  := #:db{:ident #:db{:id 17592186045430}}

  (hf-pull {:dustingetz/gender {:db/ident :db/id}} {'% 17592186045441})
  := #:dustingetz{:gender #:db{:ident #:db{:id 17592186045430}}}

  (hf-pull {:dustingetz/gender {:db/ident {:db/ident :db/ident}}} {'% 17592186045441})
  := #:dustingetz{:gender #:db{:ident #:db{:ident #:db{:ident :dustingetz/male}}}}

  (hf-pull {:dustingetz/gender :db/id} {'% 17592186045441})
  := #:dustingetz{:gender #:db{:id 17592186045430}}

  (hf-pull {:dustingetz/gender {:db/id :db/id}} {'% 17592186045441})
  := #:dustingetz{:gender #:db{:id #:db{:id 17592186045430}}}

  ; :db/id is a self reference so this actually is coherent
  (hf-pull {:dustingetz/gender {:db/id {:db/id {:db/id :db/id}}}} {'% 17592186045441})
  := #:dustingetz{:gender #:db{:id #:db{:id #:db{:id #:db{:id 17592186045430}}}}}

  )