(ns user.photon-tutorial-homework
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            devkit)
  #?(:cljs (:require-macros user.photon-tutorial-homework)) ; forces shadow hot reload to also reload JVM at the same time
  (:import (hyperfiddle.photon Pending)))


(def !x #?(:clj (atom 0)))                                  ; server

(p/defn App []
  (let [x ~@(p/watch !x)]
    (dom/div (dom/text (pr-str x)))))

(def main #?(:cljs (p/client (p/main (try (binding [dom/parent (dom/by-id "root")] (App.))
                                       (catch Pending _))))))

(comment
  #?(:clj (devkit/main :main `main))
  (swap! !x inc)

  (shadow.cljs.devtools.api/repl :app)
  (type 1)
  )




; start with a function running in the browser
; hello world
; dom hello world
; cljs atom, if odd transfer
; table, dom/for, two columns id/type, evens/odds transfer
; System/getProperties with filter
; datascript
