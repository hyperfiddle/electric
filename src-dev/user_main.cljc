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

(def home-route [::index])

(defn html5-navigate! [!path route]
  #?(:cljs (if-some [route (hf/simplify-route route)]
             (do (router/pushState! !path (contrib.ednish/encode-uri route))
                 (when-some [title (if (qualified-ident? route) route (::hf/route route))]
                   (set! js/document.title (pr-str title))))
             (router/pushState! !path "/"))))

(defn html5-replace-state! [!path route]
  #?(:cljs (router/replaceState! !path (if-some [route (hf/simplify-route route)]
                                         (contrib.ednish/encode-uri route)
                                         "/"))))

(p/defn Main []
  (try
    (hf/router
      (p/fn [!path] (or (contrib.ednish/decode-path (router/path !path) hf/read-edn-str) home-route))
      html5-navigate!
      #(.back js/window.history)
      html5-replace-state!
      (binding [dom/node (dom/by-id "root")]

        (p/server
          #_(wip.teeshirt-orders/App.)
          (user.demo-entrypoint/App. (p/client hf/route)))))

    (catch Pending _)
    (catch Cancelled e (throw e))
    (catch :default err
      (js/console.error (str (ex-message err) "\n\n" (dbg/stack-trace p/trace)) err))))
