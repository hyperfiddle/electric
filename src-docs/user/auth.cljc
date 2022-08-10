(ns user.auth
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros user.auth)))

(p/defn App []
  (dom/h1 "HTTP Basic Auth")
  (dom/p "Set login cookie here: " (dom/a {::dom/href "/auth"} "/auth") " (any user/password will do)")
  (dom/p "HTTP endpoint is here: " (dom/a {::dom/href "https://github.com/hyperfiddle/photon/blob/master/src/hyperfiddle/photon_jetty_server.clj"} "photon_jetty_server.clj"))
  (when-let [username (p/server (get-in hf/*http-request* [:cookies "username" :value]))]
    (dom/p "Authenticated as "
           (dom/span {:style {:font-weight :bold}} username))))
