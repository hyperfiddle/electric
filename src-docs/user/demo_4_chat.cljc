(ns user.demo-4-chat
  (:require
   contrib.str
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-dom2 :as dom2]
   [hyperfiddle.photon-ui4 :as ui4])
  #?(:cljs (:require-macros user.demo-4-chat)))

; A chat app.
; Open it in two tabs.
; When you type a message, both tabs update.
; This works because both tabs share a single JVM and therefore share state
; automatically. Both tabs subscribe to the same atom.

(defonce !state #?(:clj (atom []) :cljs nil))

(p/defn App []
  (p/client
    (dom/h1 "Multiplayer chat app")
    (dom/p "in 24 lines of code!")
    (dom/ul
      (p/server
        (p/for [msg (take 10 (p/watch !state))]
          (p/client
            (dom/li msg)))))
    (dom2/input (dom2/props {:placeholder "Type a message"})
      (dom2/on "keydown" (p/fn [e]
                           (when (= "Enter" (.-key e))
                             (when-some [v (contrib.str/empty->nil (.-target.value e))]
                               (p/server (swap! !state conj v))
                               (set! (.-value dom/node) ""))))))
    (ui4/button (p/fn [] (p/server (reset! !state [])))
      (dom2/text "Reset"))))
