(ns dustin.traverse
  (:require
    [clojure.walk :refer [walk #_#_prewalk postwalk]]))


(defn postwalk [f form]
  (walk (partial postwalk f) f (identity form)))

(defn prewalk [f form]
  (walk (partial prewalk f) identity (f form)))

(comment
  (def ast [1 [2 [3 [4]]]])
  (prewalk #(doto % println) ast)                           ; do the whole thing first - top down, left to right
  (postwalk #(doto % println) ast)                          ; do the whole thing last - left to right, bottom up
  )

(defn traverse [f g form]
  (walk (partial traverse f g) g (f form)))

(comment
  (def form '(inc (dec 42)))
  (traverse #(doto % println) identity form)
  (traverse identity #(doto % println) form)
  (traverse
    #(doto % println)
    #(doto % println) form)

  )

(comment

  (def ast {:dustingetz/gender :db/ident})

  (defn pull [ast v]
    (->> ast
      (traverse
        (fn [n]
          (if (keyword? n)
            (hf-nav n v)
            n))
        (fn [n]

          {k v}))))

  (pull {:dustingetz/gender :db/ident} 17592186045440)
  :=

  (pull :dustingetz/gender 17592186045440)
  := {:dustingetz/gender :male}

  (traverse
    (fn [n] (fn [v] {n (hf-nav n v)}))
    identity
    :dustingetz/gender)
  (*1 17592186045440)
  := {:dustingetz/gender :dustingetz/female}

  (traverse
    (fn [n] (fn [v] (with-meta (hf-nav n v) {:sym n})))     ; cant meta keyword
    (fn [n] (fn [v] {(:sym (meta n)) (n v)}))               ; cross k across
    :dustingetz/gender)
  (*1 17592186045440)
  := {:dustingetz/gender :dustingetz/female}

  (def ast {:dustingetz/gender :db/ident})
  (walk inner outer ast)
  (walk #(doto % println) identity ast0)
  := #:dustingetz{:gender :db/ident}
  ; [:dustingetz/gender :db/ident]

  (prewalk #(doto % println) ast)

  ((postwalk
     (fn [kf]
       (println kf)
       (if (keyword? kf)
         (fn [v]
           (hf-nav kf v))
         (constantly kf)))
     {:dustingetz/gender :db/ident})
   17592186045440)

  (postwalk
    (fn [v]
      (println v)
      (if (keyword? v)
        (fn [o] (v o))
        v))
    {:dustingetz/gender :db/ident})
  (first *1)
  (map (fn [v] (v)) *1)

  (prewalk
    (fn [n] (fn [v] {n (hf-nav n v)}))
    :dustingetz/gender)
  (*1 17592186045440)

  (prewalk
    (fn [n] (fn [v] (if (keyword? n)
                      {n (hf-nav n v)}
                      n)))
    :dustingetz/gender)
  (*1 17592186045440)

  (postwalk
    (fn [n]
      (if (keyword? n)
        {:a (doto #_(str) n println)}
        n))
    {:yo {:dustingetz/gender [:db/ident :db/ident2]}})

  ; postwalk
  ; if its a map {::gender ...}, return a new map {::gender (::gender %)}
  ; if its a vector [:db/ident], return a map {:db/ident (:db/ident ...) ...}
  ; if its a keyword :db/ident, return a map {:db/ident (:db/ident ...)}

  (postwalk
    (fn [n]
      (if (keyword? n)
        {n '...}
        n))
    {:yo {:dustingetz/gender :db/ident}})

  (prewalk
    (fn [n] (fn [v] (if (instance? clojure.lang.IMapEntry n)
                      {n (hf-nav n v)}
                      n)))
    :dustingetz/gender)
  (*1 17592186045440)

  (defn macroexpand-all [form]
    (prewalk
      (fn [x]
        (if (seq? x)
          (macroexpand x)
          x))
      form))

  (prewalk
    (fn [n]
      (println n)
      (if (keyword? n)
        (fn [v]
          {n (hf-nav n v)})
        n))
    {:dustingetz/gender :db/ident})
  (postwalk (fn [n] (if (fn? n) (n ))) *1)

  (traverse
    (fn [n]
      (println n)
      (if (keyword? n)
        (fn [v]
          (hf-nav n v))
        n))
    identity
    {:dustingetz/gender :db/ident})
  (*1 17592186045440)
  := {:dustingetz/gender {:db/ident :dustingetz/female}}

  (:dustingetz/gender (:db/ident %))
  (:db/ident (:dustingetz/gender %))
  )