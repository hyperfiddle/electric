(ns user.demo-2-toggle
  #?(:cljs (:require-macros user.demo-2-toggle))
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]))

; A stateful app with a server-side counter
; Electric functions are reactive and incrementally maintained,
; which means that when you make a small change to the state,
; the functions will recompute and you'll get a small adjustment to the DOM

(defonce !x #?(:clj (atom true) :cljs nil)) ; server state
(e/def x (e/server (e/watch !x))) ; reactive signal derived from reference

(e/defn App []
  (e/client
    (dom/h1 (dom/text "Toggle"))
    (ui/button (e/fn [] (e/server (swap! !x not)))
      (dom/text "toggle client/server"))
    (dom/p
      (dom/text "Number type is: "
        (if (e/server x)
          (e/client (pr-str (type 1))) ; javascript number type
          (e/server (pr-str (type 1)))))))) ; java number type
