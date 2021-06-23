(ns user.hello-world
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :refer [defnode hfql]]))

(s/fdef submissions :args (s/cat :first (s/nilable string?)))

(defnode homepage [needle]
  (hfql
    [{(submissions needle)
      [(:person/email ::hf/a (submission-detail %))
       {(:person/gender ::hf/options (genders))
        [:db/ident]}
       {(:person/shirt-size
          ::hf/options (shirt-sizes dustingetz/gender _))
        [:db/ident]}]}]))

(hf/pages
  [{(submissions needle)
    [(:person/email ::hf/a (submission-detail %))
     {(:person/gender ::hf/options (genders))
      [:db/ident]}
     {(:person/shirt-size ::hf/options (shirt-sizes gender _))
      [:db/ident]}]}
   {(submission-detail _)
    [:person/email
     {(:person/gender ::hf/options (genders))
      [:db/ident]}
     {(:person/shirt-size ::hf/options (shirt-sizes gender _))
      [:db/ident]}]}])


(defn foo [a]
  (let [x (inc a)
        y (dec a)]
    (+ x y)))

