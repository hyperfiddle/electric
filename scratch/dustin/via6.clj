(ns dustin.via6
  (:require
    [dustin.fiddle]
    [minitest :refer [tests]]
    [missionary.core :as m]
    [hyperfiddle.incremental :as I :refer [bindI joinI fmapI pureI capI]]
    [hyperfiddle.hfql20]))

; This fn is lifted because we are trying to test nested binds
; so we need a test fn of a -> m a
(defn submissions [a] (pureI (dustin.fiddle/submissions a)))

(tests
  (submissions "alice")
  (capI *1) := [9])

(defn render-table [>xs]
  (fmapI (fn [xs]
           [:table xs])
    >xs))

(tests
  (render-table (submissions "alice"))
  (capI *1) := [:table [9]])

(tests

  (def !route (atom [`submissions "alice"]))                ; resolve the symbol to var
  (def >route (m/watch !route))
  (def !open (atom true))
  (def >open (m/watch !open))

  ; sanity
  (capI (bindI >route pureI)) := [`dustin.via6/submissions "alice"]

  (bindI >route (fn [[fiddle needle]]
                  ((resolve fiddle) needle)))
  (capI *1) := [9]

  (let [fiddle `submissions]
    (joinI (fmapI (fn [open]
                    (case open
                      true (render-table ((resolve fiddle) "tempid"))
                      false (pureI ::nothing)))
             >open)))
  (capI *1) := [:table []]

  (let [fiddle `submissions needle "alice"]
    (fmapI vector
      (render-table ((resolve fiddle) needle))
      (joinI (fmapI (fn [open]
                      (case open
                        true (render-table ((resolve fiddle) "tempid"))
                        false (pureI ::nothing)))
               >open))))
  (capI *1) := [[:table [9]] [:table ()]]


  (bindI >route (fn [[fiddle needle]]
                  (fmapI vector
                    (render-table ((resolve fiddle) needle))
                    (joinI (fmapI (fn [open]
                                    (case open
                                      true (render-table ((resolve fiddle) "tempid"))
                                      false (pureI ::nothing)))
                             >open)))))
  (capI *1) := [[:table [9]] [:table []]]

  )
