(ns dustin.y2022.notebooks.photon-clerk
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]
            [nextjournal.clerk :as clerk]))

(hyperfiddle.rcf/enable!)

::dustin

; RCF test
(tests
  (odd? 1) := true)

; clojure
#?(:clj (type 1))

; clojurescript
#?(:cljs (type 1))



(comment
  "clerk"
  {io.github.nextjournal/clerk {:mvn/version "0.9.513"}}

  (require '[nextjournal.clerk :as clerk])

  ;; start Clerk's built-in webserver on the default port 7777,
  ;; opening the browser when done
  (clerk/serve! {:browse? true})

  ;; either call `clerk/show!` explicitly
  (clerk/show! "scratch/dustin/y2022/notebooks/rule_30.clj")
  (clerk/show! "scratch/dustin/y2022/notebooks/photon_clerk.cljc")

  ;; or let Clerk watch the given `:paths` for changes
  (clerk/serve! {:watch-paths ["notebooks" "src"]})

  ;; start with watcher and show filter function to enable notebook pinning
  (clerk/serve! {:watch-paths ["notebooks" "src"] :show-filter-fn #(clojure.string/starts-with? % "notebooks")})

  )
