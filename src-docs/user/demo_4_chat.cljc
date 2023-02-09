(ns user.demo-4-chat
  #?(:cljs (:require-macros user.demo-4-chat))
  (:import [hyperfiddle.photon Pending])
  (:require [contrib.data :refer [pad]]
            [contrib.str :refer [empty->nil]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]))

(defonce !state #?(:clj (atom (list)) :cljs nil))

(p/defn App []
  (p/client
    (dom/h1 (dom/text "Multiplayer chat app in 30 LOC"))
    (dom/ul (dom/style {:padding-left "1.5em"})
      (p/server
        (p/for-by identity [msg (reverse (pad 10 nil (p/watch !state)))]
          (p/client
            (dom/li (dom/style {:visibility (if (nil? msg) "hidden" "visible")})
              (dom/text msg))))))
    (dom/input (dom/props {:placeholder "Type a message"})
      (try
        (dom/on "keydown" (p/fn [e]
                            (when (= "Enter" (.-key e))
                              (when-some [v (empty->nil (p/fuse (-> ^js e .-target .-value)))]
                                (p/server (swap! !state #(cons v (take 9 %))))
                                (set! (.-value dom/node) "")))))
        (catch Pending _
          (dom/props {:style {:background-color "yellow"}}))))))

; A chat app. Open it in two tabs. When you type a message, both tabs update.
; This works because both tabs share a single JVM which means they subscribe to the same atom.