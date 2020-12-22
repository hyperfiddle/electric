(ns dustin.hfql6
  (:require
    [clojure.walk :refer [walk prewalk postwalk]]
    [contrib.do :refer [via* Do-via *this !]]
    [dustin.fiddle :refer :all]
    [dustin.hf-nav :refer :all]
    [meander.epsilon :as m :refer [match rewrite]]
    [minitest :refer [tests]]))


; Simple
(def ast '{(submission needle) {:dustingetz/gender :db/ident}})
(def ast {:dustingetz/gender :db/ident})
(def ast :db/ident)
(def ast '(:db/ident %))
(def ast '(submission needle))
(def ast '(submission needle))

(defn fmap [k v]                                            ; not yet a functor
  ; apply on the way in
  (let [
        result (hf-nav k v)
        ]
    ; reconstruct on the way out
    {k result}))

(comment

  {:dustingetz/gender :db/ident}
  (-> % :dustingetz/gender :db/ident) := :male
  ((comp :db/ident :dustingetz/gender) %) := :male

  ((partial hf-nav :dustingetz/gender) 17592186045440) := ::female

  (fmap :dustingetz/gender 17592186045440)
  := #:dustingetz{:gender :dustingetz/female}

    (fmap :db/ident (fmap :dustingetz/gender 17592186045440))
  := {:dustingetz/gender {:db/ident :male}}

  (fmap :db/ident %)
  := {:db/ident :male}

  )
; rewrite :asdf := ((hf-nav :asdf %))

; This is supposed to be a functor?
; What is the Functor type?
; recur with scope?
; Factor out the scope point? Higher order?

(defn apply-scope [sexp scope]
  (let [[f & asks] sexp
        args (map #(get scope %) asks)]
    (cons f args)
    #_(clojure.core/apply (clojure.core/resolve f) args)))

(defn hf-pull [pat v scope]                                 ; remove v by put % in scope
  #_{:pre  [(doto pat (println 'hf-pull v scope))]
     :post [(doto % (println 'hf-pull))]}
  (match pat
    (m/pred keyword? ?k)
    {?k (hf-nav ?k v)}

    (!xs ...)
    {(seq !xs) (apply-scope !xs scope)}

    {& (m/seqable [?kf ?pat])}                              ; one entry
    (let [v (hf-pull ?kf v scope)
          scope (merge scope {?kf v})
          v (hf-pull ?pat v scope)]
      {?k v})

    ;{& (m/seqable [(!xs ...) ?pat])}
    ;(let [v (apply-scope !xs scope)
    ;      scope (merge scope {f v})
    ;      v (hf-pull ?pat v scope)]
    ;  {(seq !xs) v})

    ?_ (doto ?_ (println 'unmatched))
    ))

; defmacro hf-pull?
; how much of this can be a rewrite , known statically?
; The shape actually is static in the cardinality-1 case
; so can we compile to something static
; or does the evaluation depend on the database entity