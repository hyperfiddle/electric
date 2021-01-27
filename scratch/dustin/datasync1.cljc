(ns dustin.datasync1
  (:require
    [hyperfiddle.api :as hf]
    [hyperfiddle.incremental :refer [fmapI capI bindI]]
    [minitest :refer [tests]]
    [missionary.core :as m]
    [datascript.core :as d]
    [dustin.fiddle :refer [submissions]]))

(declare something-else)

; we want to make two reactors communicate over wire
; communicate info about what happens at arbitrary points of ast

(defn render [xs]
  [:table
   [:tr xs]
   #_(rfor [x :db/id xs]
       [:tr (pr-str x)])])

(tests
  (render []) := [:table [:tr []]])

(defn query-route [>$ [f & args :as route]]
  (case f

    dustin.fiddle/submissions
    (let [[needle] args]
      (fmapI #(binding [hf/*$* %]
                (submissions needle))
        >$))

    something-else
    nil))

(defn router [>$ >route]
  (fmapI render (bindI >route #(query-route >$ %))))

(tests
  (def !route (atom ['dustin.fiddle/submissions "alice"]))
  (def !$ (atom hf/*$*))
  (def >route (m/watch !route))
  (def >$ (m/watch !$))

  (query-route >$ ['dustin.fiddle/submissions "alice"])
  (capI *1) := [9]

  (router >$ >route)
  (capI *1) := [:table [:tr [9]]]

  (do
    (reset! !$
      (:db-after (d/with @!$ [{:dustingetz/email      "don@example.com"
                               :dustingetz/gender     :dustingetz/male
                               :dustingetz/shirt-size :dustingetz/mens-large}])))
    nil)
  (router >$ >route)
  (capI *1) := [:table [:tr [9]]]

  )


