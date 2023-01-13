(ns user.demo-4-chat
  (:require
   contrib.str
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom1]
   [hyperfiddle.photon-dom2 :as dom]
   [hyperfiddle.photon-ui4 :as ui4])
  #?(:cljs (:require-macros user.demo-4-chat)))

; A chat app.
; Open it in two tabs.
; When you type a message, both tabs update.
; This works because both tabs share a single JVM and therefore share state
; automatically. Both tabs subscribe to the same atom.

(defonce !state #?(:clj (atom '()) :cljs nil))

(p/defn App []
  (p/client
    (dom/h1 (dom/text "Multiplayer chat app"))
    (dom/p (dom/text "in 24 lines of code!"))
    (dom/ul
      (p/server
        (p/for [msg (reverse (p/watch !state))]
          (p/client
            (dom/li (dom/text msg))))))
    (dom/input (dom/props {:placeholder "Type a message"})
      (dom/on "keydown" (p/fn [e]
                           (when (= "Enter" (.-key e))
                             (when-some [v (contrib.str/empty->nil (.-target.value e))]
                               (p/server (swap! !state #(cons v (take 9 %))))
                               (set! (.-value dom1/node) ""))))))
    (ui4/button (p/fn [] (p/server (reset! !state [])))
      (dom/text "Reset"))))
