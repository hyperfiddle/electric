(ns dustin.datasync1
  (:require
    [hyperfiddle.api :as hf]
    [hyperfiddle.incremental :refer [fmapI capI bindI]]
    [minitest :refer [tests]]
    [missionary.core :as m]
    [datascript.core :as d]
    [dustin.fiddle :as f]
    [dustin.compiler1 :refer [dataflow log!]]))

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

(defn submissions [$ needle]
  (binding [hf/*$* $]
    (f/submissions needle)))

(defn query-route [>$ [f & args :as route]]
  (case f

    dustin.fiddle/submissions
    (let [[needle] args]
      (fmapI #(submissions % needle) >$))

    404))

(defn router [>$ >route]
  (fmapI render (bindI >route #(query-route >$ %))))

(defn source-index [ast form]
  (.indexOf (tree-seq coll? identity ast) form))

(tests
  (def !route (atom nil))
  (def !$ (atom hf/*$*))
  (def >route (m/watch !route))
  (def >$ (m/watch !$))
  (def qr (partial query-route >$))
  (def ast '(fmap render (bind >route qr)))
  (source-index ast '>route) := 5
  (source-index ast '(bind >route qr)) := 3

  (def d (dataflow (fmap render (bind >route qr))))
  (def !trace (log! d))

  (reset! !route ['dustin.fiddle/submissions "alice"])
  (subs hf/*$* "alice")

  @!trace :=
  [{[5] ['dustin.fiddle/submissions "alice"]
    [3] '(9)
    [0] [:table [:tr '(9)]]}]

  ;; TODO
  ;; * make test above pass
  ;; * split across client & server
  
  )


(comment

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