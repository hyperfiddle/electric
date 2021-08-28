(ns user.gender-shirt-size
  (:require [clojure.spec.alpha :as s]
            [hfdl.lang :as p]
            [hfdl.impl.util :as u]
            [hyperfiddle.api :as hf :refer [*$*]]
            #_[hyperfiddle.q2 :as q :refer [q nav hfql]]
            [hyperfiddle.rcf :refer [tests ! %]]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs
     (:require-macros [user.gender-shirt-size :refer [ref2 render-form page-submissions]])))

(p/defn genders []
  (into [] ~(hf/q '[:find [?e ...] :where [_ :dustingetz/gender ?e]])))

(tests
  (def dispose (p/run (! (p/$ genders))))
  % := [:dustingetz/male :dustingetz/female]
  (dispose))

(p/defn gender []
  (first (p/$ genders)))

(tests
  (def dispose (p/run (! (p/$ gender))))
  % := :dustingetz/male
  (dispose))

(p/defn shirt-sizes [gender needle]
  (sort
    ~(hf/q '[:in $ % ?gender ?needle
          :find [?e ...]
          :where
          [?e :dustingetz/type :dustingetz/shirt-size]
          [?e :dustingetz/gender ?gender]
          [?e :db/ident ?ident]
          (hyperfiddle.api/needle-match ?ident ?needle)]
       hf/rules gender (or needle ""))))

(s/fdef shirt-sizes :args (s/cat :gender keyword?
                                 :needle string?) :ret sequential?)

(p/defn submissions [needle]
  (sort
    ~(hf/q '[:find [?e ...]
          :in $ % ?needle
          :where
          [?e :dustingetz/email ?email]
          (hyperfiddle.api/needle-match ?email ?needle)]
       hf/rules (or needle ""))))

(s/fdef submissions :args (s/cat :needle string?)
        :ret (s/coll-of (s/keys :req [:dustingetz/email
                                      :dustingetz/email1
                                      :dustingetz/gender
                                      :dustingetz/shirt-size])))

(p/defn submission [needle] (first (p/$ submissions needle)))

(tests
  (hfdl.lang/run (! (p/$ user.gender-shirt-size/submission "")))
  %)

(s/fdef submissions :args (s/cat :needle string?) :ret sequential?)

(tests
  (def dispose
    (p/run
      (! (p/$ submissions ""))
      (! (p/$ submissions "example"))
      (! (p/$ submissions "b"))))
  % := [9 10 11]
  % := [9 10 11]
  % := [10]
  (dispose))

(comment
  (p/defn ref2 [v {::hf/keys [options]} needle]
    (dom/select v (p/for [x (options "")] (dom/option x))))

  ;(defmacro ref3 [v {::hf/keys [options]}]
  ;  `(let [gender nil
  ;         needle ""]
  ;     (dom/select ~v (p/for [x ~options] (dom/option x)))))

  (defmacro auto-render [v props]
    ;(let [!needle (atom "")])
    `(dom/div
       (dom/h1 "submissions")
       #_(let [args
               (dom/form
                 (dom/field (dom/span "needle") (dom/input needle))
                 (dom/field (dom/span "needle2") (dom/input needle2))
                 (dom/field (dom/span "needle3") (dom/input needle3)))]) ; in scope
       (let [xs ~v #_(v needle)]                            ; assume needle in scope
         (dom/table
           (p/for [{:keys [:db/id
                           :dustingetz/email
                           :dustingetz/gender
                           :dustingetz/shirt-size]} xs #_(xs ~(m/watch !needle))]
             (dom/tr
               (dom/td (str id))
               (dom/td email)
               (dom/td (pr-str gender))
               (dom/td (pr-str shirt-size))))))))

  (p/defn render-text [x opts]
    (dom/input x))

  (p/defn page-submissions []
    (hfql
      [{((submissions "") ::hf/render auto-render)
        [:db/id
         (:dustingetz/email ::hf/render render-text)
         {(:dustingetz/gender ::hf/options (genders) ::hf/render ref2) [:db/ident]}
         {(:dustingetz/shirt-size ::hf/options (shirt-sizes :dustingetz/male "")
            ::hf/render ref2) [:db/ident]}]}])))
