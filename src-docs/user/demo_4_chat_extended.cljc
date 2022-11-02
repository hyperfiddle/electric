(ns user.demo-4-chat-extended
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [missionary.core :as m])
  #?(:cljs (:require-macros user.demo-4-chat-extended)))

; Fleshed out chat demo with auth and presence
; Has missionary interop, this is a more advanced demo

(defonce !msgs #?(:clj (atom []) :cljs nil))
(p/def msgs (p/server (p/watch !msgs)))

(defonce !present #?(:clj (atom {}) :cljs nil)) ; session-id -> user
(p/def present (p/server (p/watch !present)))

(p/defn Chat [username]
  (dom/p "Present: ")
  (dom/ul
    (p/server
      (p/for [[session-id username] present]
        (p/client
          (dom/li username (str " (" session-id ")"))))))

  (dom/hr)
  (ui/input {::dom/type "text"
             ::dom/placeholder "Type a message"
             ::ui/keychords #{"enter"}
             ::ui/keychord-event (p/fn [e]
                                   (let [v (:value dom/node)]
                                     (p/server (swap! !msgs conj {::username username ::msg v})))
                                   (set! (.. e -target -value) ""))})
  (dom/ul
    (p/server
      (p/for [{:keys [::username ::msg]} (take 10 msgs)]
        (p/client
          (dom/li
            (dom/strong username) " " msg))))))

(p/defn App []
  (p/client
    (dom/h1 "Multiplayer chat app with auth and presence")
    (let [session-id (p/server (get-in hf/*http-request* [:headers "sec-websocket-key"]))
          username (p/server (get-in hf/*http-request* [:cookies "username" :value]))]
      (if-not (some? username)
        (do (dom/p "Set login cookie here: " (dom/a {::dom/href "/auth"} "/auth") " (blank password)")
            (dom/p "Example HTTP endpoint is here: "
              (dom/a {::dom/href "https://github.com/hyperfiddle/photon/blob/master/src/hyperfiddle/photon_jetty_server.clj"}
                     "photon_jetty_server.clj")))
        (do
          (p/server
            ; >x is a missionary flow that attaches mount/unmount side effect
            ; to the lifecycle of the flow
            (let [>x (->> (m/observe (fn mount [!]
                                       (println `mount username session-id)
                                       (swap! !present assoc session-id username)
                                       (fn unmount []
                                         (println `unmount username session-id)
                                         (swap! !present dissoc session-id))))
                          (m/reductions {} nil))]
              ; missionary flows are mounted with `new` (monadic join).
              ; This works because Photon compiles to missionary so this actually typechecks
              ; from a compiler internals perspective.
              (new >x)))
          (dom/p "Authenticated as: " username)
          (Chat. username))))))
