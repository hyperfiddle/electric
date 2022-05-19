(ns user.photon-live-demo
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros user.photon-live-demo))
  (:import (hyperfiddle.photon Pending)))

(def !x #?(:cljs (atom 0)))                                  ; server

(p/defn App []
  (let [x (p/watch !x)]
    (dom/div
      (dom/h1 (dom/text "Toggle Server"))
      (dom/div (dom/text (if (odd? x)
                           ~@(pr-str (type x))
                           (pr-str (type x))))))))

(def main #?(:cljs (p/client (p/main
                               (try (binding [dom/parent (dom/by-id "root")]
                                      (App.))
                                 (catch Pending _))))))

(comment
  (shadow.cljs.devtools.api/repl :devkit)
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
