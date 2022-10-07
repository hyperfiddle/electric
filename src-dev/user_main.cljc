(ns ^:dev/always ; force rebuild here? We don't understand why
  user-main
  (:require contrib.uri ; data_readers
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon.debug :as dbg]
            [hyperfiddle.photon-dom :as dom]
            user.demo-entrypoint)
  (:import [hyperfiddle.photon Pending]
           [missionary Cancelled])
  #?(:cljs (:require-macros user-main)))

; application main is a separate .cljc file because p/server is not valid in user.cljs.

(p/defn Main []
  (try
    (binding [dom/node (dom/by-id "root")]
      (dom/div {}
        (p/server ; Demos are server biased.
          ; typical web app information flow is server -> client,
          ; so encouraging userland fns to "start" on the server helps avoid
          ; accidental unserializable reference transfer issues until we make the
          ; transfer semantics more intuitive.
          (user.demo-entrypoint/App.))))
    (catch Pending _)
    (catch Cancelled e (throw e))
    (catch :default err
      (js/console.error (str (ex-message err) "\n\n" (dbg/stack-trace p/trace)) err)
      #_(throw err))))
