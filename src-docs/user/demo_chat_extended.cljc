(ns user.demo-chat-extended
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]))

; Fleshed out chat demo with auth and presence

#?(:clj (defonce !msgs (atom (list))))
(e/def msgs (e/server (reverse (e/watch !msgs))))

#?(:clj (defonce !present (atom {}))) ; session-id -> user
(e/def present (e/server (e/watch !present)))

(e/defn Chat-UI [username]
  (dom/p (dom/text "Present: "))
  (dom/ul
    (e/server
      (e/for-by first [[session-id username] present]
        (e/client
          (dom/li (dom/text username (str " (session-id: " session-id ")")))))))

  (dom/hr)
  (dom/ul
    (e/server
      (e/for [{:keys [::username ::msg]} msgs]
        (e/client
          (dom/li (dom/strong (dom/text username))
            (dom/text " " msg))))))

  (dom/input
    (dom/props {:placeholder "Type a message"})
    (ui/on-submit (e/fn [v]
                    (dom/style {:background-color "yellow"})
                    (e/server (swap! !msgs #(cons {::username username ::msg v} (take 9 %))))))))

(e/defn ChatExtended []
  (e/client
    (dom/h1 (dom/text "Multiplayer chat app with auth and presence"))
    (let [session-id (e/server (get-in e/*http-request* [:headers "sec-websocket-key"]))
          username (e/server (get-in e/*http-request* [:cookies "username" :value]))]
      (if-not (some? username)
        (do (dom/p (dom/text "Set login cookie here: ") (dom/a (dom/props {::dom/href "/auth"}) (dom/text "/auth")) (dom/text " (blank password)"))
            (dom/p (dom/text "Example HTTP endpoint is here: ")
              (dom/a (dom/props {::dom/href "https://github.com/hyperfiddle/electric/blob/master/src/hyperfiddle/electric_jetty_server.clj"})
                (dom/text "electric_jetty_server.clj"))))
        (do
          (e/server
            (swap! !present assoc session-id username)
            (e/on-unmount #(swap! !present dissoc session-id)))
          (dom/p (dom/text "Authenticated as: " username))
          (Chat-UI. username))))))
