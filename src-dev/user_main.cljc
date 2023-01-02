(ns ^:dev/always ; force rebuild here? We don't understand why
  user-main
  #?(:cljs (:require-macros user-main))
  (:import [hyperfiddle.photon Pending]
           [missionary Cancelled])
  (:require contrib.sexpr-router
            contrib.ednish
            contrib.uri ; data_readers
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon.debug :as dbg]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.router :as router]
            [missionary.core :as m]
            user.demo-entrypoint))

; application main is a separate .cljc file because p/server is not valid in user.cljs.

(def home-route [::index])

(p/defn Main []
  (try
    (let [!path (m/mbx)]
      (binding [hf/route (contrib.sexpr-router/decode (router/path !path) home-route)
                router/Link (router/->Link. !path contrib.sexpr-router/encode)
                dom/node (dom/by-id "root")]

        (p/server
          #_(wip.teeshirt-orders/App.)
          (user.demo-entrypoint/App. (p/client hf/route)))))

    (catch Pending _)
    (catch Cancelled e (throw e))
    (catch :default err
      (js/console.error (str (ex-message err) "\n\n" (dbg/stack-trace p/trace)) err))))
