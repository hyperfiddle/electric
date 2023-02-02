(ns ^:dev/always ; force rebuild here? We don't understand why
  user-main
  #?(:cljs (:require-macros user-main))
  (:import [hyperfiddle.photon Pending]
           [missionary Cancelled])
  (:require contrib.ednish
            contrib.uri ; data_readers
            clojure.string
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon.debug :as dbg]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.router :as router]
            #?(:cljs [hyperfiddle.router-html5 :as html5])
            user.demo-entrypoint))

; application main is a separate .cljc file because p/server is not valid in user.cljs.

(def home-route {::hf/route [::index]})

(defn simplify-route [route]
  (if (and (= 1 (count route)) (contains? route ::hf/route))
    (::hf/route route)
    route))

(defn parse-route [route]
  (if (map? route)
    route
    {::hf/route route}))

#?(:cljs
   (defn set-page-title! [route]
     (set! (.-title js/document) (str (clojure.string/capitalize (name (first (::hf/route route)))) " - Hyperfiddle"))))

(p/defn Main []
  (try
    (binding [router/encode (comp contrib.ednish/encode-uri simplify-route)
              router/decode #(parse-route (or (contrib.ednish/decode-path % hf/read-edn-str) home-route))]
      (router/router (html5/HTML5-History.)
        (set-page-title! router/route)
        (binding [dom/node js/document.body]
          (p/server
            (user.demo-entrypoint/App. (p/client router/route))))))

    (catch Pending _)
    (catch Cancelled e (throw e))
    (catch :default err
      (js/console.error (str (ex-message err) "\n\n" (dbg/stack-trace p/trace)) err))))
