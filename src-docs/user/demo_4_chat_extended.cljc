(ns user.demo-4-chat-extended
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [missionary.core :as m])
  #?(:cljs (:require-macros user.demo-4-chat-extended)))

; I'm making a multiplayer game with chat and I'd like to implement presence (just a list of
; connected users). So a user can enter a name to join, I add the name in an atom on the server and
; all clients can see who is in the chat. This works fine, but I need to remove names from the set
; when they leave the page or have some way to check if a client is still connected to update the
; list

(defonce !msgs #?(:clj (atom []) :cljs nil))
(p/def msgs (p/server (p/watch !msgs)))

(defonce !present #?(:clj (atom #{}) :cljs nil))
(p/def present (p/server (p/watch !present)))

(p/defn Chat [username]
  (dom/p "Present: ")
  (dom/ul
    (p/server
      (p/for [user present]
        (p/client
          (dom/li user)))))

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
    (dom/p "Example HTTP endpoint is here: "
      (dom/a {::dom/href "https://github.com/hyperfiddle/photon/blob/master/src/hyperfiddle/photon_jetty_server.clj"}
             "photon_jetty_server.clj"))
    (let [username (p/server (get-in hf/*http-request* [:cookies "username" :value]))]
      (if-not (some? username)
        (dom/p "Set login cookie here: " (dom/a {::dom/href "/auth"} "/auth") " (any user/password will do)")
        (do
          (p/server (new (->> (m/observe (fn mount [!]
                                           (println `mount username)
                                           (swap! !present conj username)
                                           (fn unmount []
                                             (println `unmount username)
                                             (swap! !present disj username))))
                              (m/reductions {} nil))))
          (dom/p "Authenticated as: " username)
          (Chat. username))))))
