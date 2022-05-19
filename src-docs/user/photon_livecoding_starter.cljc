(ns user.photon-livecoding-starter
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.photon-livecoding-starter))) ; forces shadow hot reload to also reload JVM at the same time


(def !x #?(:clj (atom 0)))                                  ; server

(p/defn App []
  (let [x ~@(p/watch !x)]
    (dom/div
      (dom/h1 (dom/text "Toggle Server"))
      (dom/div (dom/text (if (odd? x)
                           ~@(pr-str (type x))
                           (pr-str (type x))))))))

(def main #?(:cljs (p/client (p/main
                               (binding [dom/parent (dom/by-id "root")]
                                 (try
                                   (App.)
                                   (catch Pending _)))))))

(comment
  #?(:clj (user/browser-main! `main))
  (shadow.cljs.devtools.api/repl :app)
  (swap! !x inc)
  )






; start with a function running in the browser
; hello world
; dom hello world
; table with dom/for
; cljs atom
; transfer java lang long
; table of evens long, with id column and type column
; System/getProperties with filter
; datascript
