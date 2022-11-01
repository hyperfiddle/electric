(ns user.demo-4-chat
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  #?(:cljs (:require-macros user.demo-4-chat)))

(defonce !state #?(:clj (atom []) :cljs nil))
(p/def state (p/server (p/watch !state)))

(p/defn App []
  (p/client
    (dom/h1 "Multiplayer chat app")
    (dom/p "in 24 lines of code!")
    (ui/input {::dom/type "text"
               ::dom/placeholder "Type a message"
               ::ui/keychords #{"enter"}
               ::ui/keychord-event (p/fn [e]
                                     (let [v (:value dom/node)]
                                       (p/server (swap! !state conj v)))
                                     (set! (.. e -target -value) ""))})
    (dom/ul
      (p/server
        (p/for [msg (take 10 state)]
          (p/client
            (dom/li msg)))))))
