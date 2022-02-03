(ns dustin.hfql10
  "pintfree scope"
  (:require
    [clojure.walk :refer [walk prewalk postwalk]]
    [contrib.do :refer [via* Do-via *this !]]
    [datomic.api :as d]
    [dustin.fiddle :refer :all]
    [dustin.hf-nav :refer :all]
    [hyperfiddle.api :as hf]
    [meander.epsilon :as m :refer [match rewrite]]
    [minitest :refer [tests]]))


(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(defn hf-eval [edge scope]
  (let [v (cond
            (keyword? edge) (hf-nav edge (get scope '%))
            (seq? edge)     (let [[f & args] edge]
                              (clojure.core/apply (clojure.core/resolve f) (replace scope args)))
            () (println "hf-eval unmatched edge: " edge))]
    (merge scope {(hf-edge->sym edge) v '% v})))

(tests
  (hf-eval :dustingetz/gender {'% 17592186045441})
  := '{% :dustingetz/male, dustingetz/gender :dustingetz/male}

  (hf-eval '(identity %) {'% 17592186045441})
  := '{% 17592186045441, (identity %) 17592186045441}
  )

(defn hf-pull [pat]
  (match pat

    {& (m/seqable [?edge ?pat])}                            ; one entry
    {?edge (fn [scope]
             #_(println 'deep ?edge scope)
             (run-scope (hf-pull ?pat) (hf-eval ?edge scope)))}

    ?leaf                                                   ; :dustingetz/gender '(f dustingetz/gender)
    {?leaf (fn [scope]
             (-> (hf-eval ?leaf scope) (get '%)))}
    ))

(tests
  (hf-pull :db/ident) := #:db{:ident '(fn [scope] ...)}     ; a thunked tree, feed it scopes while traversing
  ((:db/ident *1) {'% :dustingetz/male}) := :dustingetz/male
  )

(defn run-scope [tree scope]
  ; traverse it, feeding a scope as we go
  ; (sort of like macroexpand-all)
  (->> tree
    (prewalk (fn [tree]
               #_{:pre [(doto tree println)]
                :post [(doto % println)]}
               (if (fn? tree)
                 (tree scope)
                 tree)))))

(tests

  (run-scope (hf-pull :db/ident) {'% :dustingetz/male})
  := #:db{:ident :dustingetz/male}

  ;(hf-pull '(:db/ident %) {'% :dustingetz/male})            ; interpret kw as entity nav? There's no need to, don't do this
  ;:= {(:db/ident %) (:db/ident :dustingetz/male)} ; ClassCastException

  (run-scope (hf-pull '(hf-nav :db/ident %)) {'% :dustingetz/male})
  := {'(hf-nav :db/ident %) :dustingetz/male}

  (run-scope (hf-pull '(identity %)) {'% :dustingetz/male})
  := {'(identity %) :dustingetz/male}

  (run-scope (hf-pull :dustingetz/gender) {'% 17592186045441})
  := #:dustingetz{:gender :dustingetz/male}

  ;(hf-pull '(:dustingetz/gender %) {'% 17592186045441})
  ;:= {(:dustingetz/gender %) (:dustingetz/gender 17592186045441)}

  (run-scope (hf-pull {:dustingetz/gender :db/ident}) {'% 17592186045441})
  := #:dustingetz{:gender #:db{:ident :dustingetz/male}}

  (run-scope (hf-pull '(gender)) {})
  := '{(gender) 17592186045430}

  (run-scope (hf-pull '(submission needle)) {'needle "alic"})
  := '{(submission needle) 17592186045440}

  (run-scope (hf-pull {'(submission needle) :dustingetz/gender}) {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender :dustingetz/female}}

  (run-scope (hf-pull {'(submission needle) {:dustingetz/gender :db/ident}}) {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender #:db{:ident :dustingetz/female}}}

  (run-scope (hf-pull '{(submission needle) {:dustingetz/gender (shirt-size dustingetz/gender)}}) {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045436}}}

  (run-scope (hf-pull '{(submission needle) {:dustingetz/gender {(shirt-size dustingetz/gender) :db/ident}}}) {'needle "alic"})
  := '{(submission needle) #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/womens-small}}}}

  (run-scope (hf-pull {:db/ident :db/id}) {'% 17592186045430})
  := #:db{:ident #:db{:id 17592186045430}}

  (run-scope (hf-pull {:dustingetz/gender {:db/ident :db/id}}) {'% 17592186045441})
  := #:dustingetz{:gender #:db{:ident #:db{:id 17592186045430}}}

  (run-scope (hf-pull {:dustingetz/gender {:db/ident {:db/ident :db/ident}}}) {'% 17592186045441})
  := #:dustingetz{:gender #:db{:ident #:db{:ident #:db{:ident :dustingetz/male}}}}

  (run-scope (hf-pull {:dustingetz/gender :db/id}) {'% 17592186045441})
  := #:dustingetz{:gender #:db{:id 17592186045430}}

  (run-scope (hf-pull {:dustingetz/gender {:db/id :db/id}}) {'% 17592186045441})
  := #:dustingetz{:gender #:db{:id #:db{:id 17592186045430}}}

  ; :db/id is a self reference so this actually is coherent
  (run-scope (hf-pull {:dustingetz/gender {:db/id {:db/id {:db/id :db/id}}}}) {'% 17592186045441})
  := #:dustingetz{:gender #:db{:id #:db{:id #:db{:id #:db{:id 17592186045430}}}}}

  )