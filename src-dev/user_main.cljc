(ns ^:dev/always ; force rebuild here? We don't understand why
  user-main
  #?(:cljs (:require-macros user-main))
  (:import [hyperfiddle.photon Pending]
           [missionary Cancelled])
  (:require contrib.ednish
            contrib.uri ; data_readers
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon.debug :as dbg]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.router :as router]
            [missionary.core :as m]
            user.demo-entrypoint))

; application main is a separate .cljc file because p/server is not valid in user.cljs.

#?(:cljs (defn decode-path [path read-edn-str]
           {:pre [(string? path) (some? read-edn-str)]}
           (case path
             "/" ::index
             (-> path (subs 1) contrib.ednish/decode read-edn-str))))

#?(:cljs (defn encode-path [route] (->> route pr-str contrib.ednish/encode (str "/"))))

(p/defn Main []
  (try
    (let [!path (m/mbx)
          route (decode-path (router/path !path) hf/read-edn-str)]
      (binding [router/Link (router/->Link. !path encode-path)
                dom/node (dom/by-id "root")]

        (p/server
          (user.demo-entrypoint/App. route))))

    (catch Pending _)
    (catch Cancelled e (throw e))
    (catch :default err
      (js/console.error (str (ex-message err) "\n\n" (dbg/stack-trace p/trace)) err))))
