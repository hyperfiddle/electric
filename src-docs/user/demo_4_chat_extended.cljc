(ns user.demo-4-chat-extended
  #?(:cljs (:require-macros user.demo-4-chat-extended))
  (:require
   contrib.str
   [hyperfiddle.api :as hf]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [missionary.core :as m]))

; Fleshed out chat demo with auth and presence
; Has missionary interop, this is a more advanced demo

(defonce !msgs #?(:clj (atom '()) :cljs nil))
(e/def msgs (e/server (reverse (e/watch !msgs))))

(defonce !present #?(:clj (atom {}) :cljs nil)) ; session-id -> user
(e/def present (e/server (e/watch !present)))

(e/defn Chat [username]
  (dom/p (dom/text "Present: "))
  (dom/ul
    (e/server
      (e/for [[session-id username] present]
        (e/client
          (dom/li (dom/text username (str " (session-id: " session-id ")")))))))

  (dom/hr)
  (dom/ul
    (e/server
      (e/for [{:keys [::username ::msg]} msgs]
        (e/client
          (dom/li
            (dom/strong (dom/text username)) (dom/text " " msg))))))

  (dom/input
    (dom/props {:placeholder "Type a message"})
    (dom/on "keydown" (e/fn [e]
                        (when (= "Enter" (.-key e))
                          (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                            (dom/style {:background-color "yellow"})
                            (e/server (swap! !msgs #(cons {::username username ::msg v} (take 9 %))))
                            (set! (.-value dom/node) "")))))))

(e/defn App []
  (e/client
    (dom/h1 (dom/text "Multiplayer chat app with auth and presence"))
    (let [session-id (e/server (get-in hf/*http-request* [:headers "sec-websocket-key"]))
          username (e/server (get-in hf/*http-request* [:cookies "username" :value]))]
      (if-not (some? username)
        (do (dom/p (dom/text "Set login cookie here: ") (dom/a (dom/props {::dom/href "/auth"}) (dom/text "/auth")) (dom/text " (blank password)"))
            (dom/p (dom/text "Example HTTP endpoint is here: ")
              (dom/a (dom/props {::dom/href "https://github.com/hyperfiddle/electric/blob/master/src/hyperfiddle/electric_jetty_server.clj"})
                (dom/text "electric_jetty_server.clj"))))
        (do
          (e/server
            ; >x is a Missionary flow that attaches side effect to the mount/unmount lifecycle
            (let [>x (->> (m/observe (fn mount [!]
                                       (println `mount username session-id)
                                       (swap! !present assoc session-id username)
                                       (fn unmount []
                                         (println `unmount username session-id)
                                         (swap! !present dissoc session-id))))
                          (m/reductions {} nil))]
              ; Missionary flows are booted with `new` (monadic join)
              ; This works because Electric is essentially a Clojure-to-Missionary compiler,
              ; so this actually typechecks from a compiler internals perspective.
              (new >x)))
          (dom/p (dom/text "Authenticated as: " username))
          (Chat. username))))))
