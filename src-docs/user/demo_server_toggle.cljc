(ns user.demo-server-toggle
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            devkit)
  #?(:cljs (:require-macros user.demo-server-toggle)))


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
                                 (App.))))))

(comment
  #?(:clj (devkit/main :main `main))
  (swap! !x inc)

  (shadow.cljs.devtools.api/repl :app)
  (type 1)
  )
