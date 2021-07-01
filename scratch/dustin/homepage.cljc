(ns user.hello-world
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.api :as hf]))

(defn submissions [needle] ...)
(defn genders [] ...)
(defn shirt-sizes [gender needle] ...)

(s/fdef submissions :args (s/cat :needle string?) :ret sequential?)
(s/fdef genders :args (s/cat) :ret sequential?)
(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?) :ret sequential?)

(defnode page [needle]
  (hfql
    {(submissions needle)
     [:db/id
      :person/email
      {(:person/gender ::hf/options (genders))
       [:db/ident]}
      {(:person/shirt-size ::hf/options (shirt-sizes gender .))
       [:db/ident]}]}))


(hf/serve
  [{(submissions .)
    [:db/id
     :person/email
     {(:person/gender ::hf/options (genders))
      [:db/ident]}
     {(:person/shirt-size ::hf/options (shirt-sizes gender .))
      [:db/ident]}]}])
