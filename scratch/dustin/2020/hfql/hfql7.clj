(ns dustin.hfql7
  (:require
    [clojure.walk :refer [walk prewalk postwalk]]
    [contrib.do :refer [via* Do-via *this !]]
    [datomic.api :as d]
    [dustin.fiddle :refer :all]
    [dustin.hf-nav :refer :all]
    [hyperfiddle.api :as hf]
    [meander.epsilon :as m :refer [match rewrite]]
    [minitest :refer [tests]]))


(defn apply-scope [sexp scope]
  (let [[f & asks] sexp
        args (map #(get scope %) asks)]
    (cons f args)
    #_(clojure.core/apply (clojure.core/resolve f) args)))

(defn hf-pull [pat v scope]                                 ; remove v by put % in scope
  (match pat
    (m/pred keyword? ?k)
    {?k (hf-nav ?k v)}

    {& (m/seqable [?edge ?pat])}                            ; one entry
    (let [v (hf-nav ?edge v)
          scope (merge scope {?edge v})]
      {?edge (hf-pull ?pat v scope)})

    ?_ (doto ?_ (println 'unmatched))
    ))

(tests

  ; red herring base case? why would this produce a map
  (hf-pull :db/ident :dustingetz/male {})
  := #:db{:ident :dustingetz/male}

  (hf-pull :dustingetz/gender 17592186045441 {})
  := #:dustingetz{:gender :dustingetz/male}                 ; smart ref

  ; the map determines the descent
  (hf-pull {:dustingetz/gender :db/ident} 17592186045441 {})
  := #:dustingetz{:gender #:db{:ident :dustingetz/male}}

  (hf-pull {:db/ident :db/id} 17592186045430 {})
  := #:db{:ident {:db/id 17592186045430}}

  (hf-pull {:dustingetz/gender {:db/ident :db/id}} 17592186045441 {})
  := #:dustingetz{:gender #:db{:ident {:db/id 17592186045430}}}

  (hf-pull {:dustingetz/gender {:db/ident {:db/ident :db/ident}}} 17592186045441 {})
  := #:dustingetz{:gender #:db{:ident #:db{:ident #:db{:ident :dustingetz/male}}}}

  (hf-pull {:dustingetz/gender :db/id} 17592186045441 {})
  := #:dustingetz{:gender #:db{:id 17592186045430}}

  (hf-pull {:dustingetz/gender {:db/id :db/id}} 17592186045441 {})
  := #:dustingetz{:gender #:db{:id #:db{:id 17592186045430}}}

  ; :db/id is a self reference so this actually is coherent
  (hf-pull {:dustingetz/gender {:db/id {:db/id {:db/id :db/id}}}} 17592186045441 {})
  := #:dustingetz{:gender #:db{:id #:db{:id #:db{:id 17592186045430}}}}

  )