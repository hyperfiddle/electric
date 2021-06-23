(ns user.hello-world
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]))

(defn submissions [needle] ...)
(defn genders [] ...)
(defn shirt-sizes [gender needle] ...)

(s/fdef submissions :args (s/cat :needle string?))
(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?))

(hf/serve
  [{(submissions _)
    [:db/id
     :person/email
     {(:person/gender ::hf/options (genders))
      [:db/ident]}
     {(:person/shirt-size ::hf/options (shirt-sizes gender _))
      [:db/ident]}]}])

(defn foo [a]
  (+ (inc a) (dec a)))
