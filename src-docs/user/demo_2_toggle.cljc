(ns user.demo-2-toggle
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-dom2 :as dom2]
   [hyperfiddle.photon-ui4 :as ui])
  #?(:cljs (:require-macros user.demo-2-toggle)))

; A stateful app with a server-side counter
; Photon functions are reactive and incrementally maintained,
; which means that when you make a small change to the state,
; the functions will recompute and you'll get a small adjustment to the DOM

(defonce !x #?(:clj (atom true) :cljs nil)) ; mutable reference on server
(p/def x (p/server (p/watch !x))) ; reactive x, attached to reference !x

(p/defn App []
  (p/client
    (dom/h1 "Toggle")
    ;; button is auto-disabled while the 'task' is pending
    (ui/button (p/fn [] (p/server (swap! !x not)))
      (dom2/text "toggle client/server"))
    (dom/p
      "Number type is: "
      (if (p/server x)
        (p/client (pr-str (type 1)))           ; javascript number type
        (p/server (pr-str (type 1)))))))       ; java number type
