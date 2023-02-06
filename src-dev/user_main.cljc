(ns ^:dev/always ; recompile Photon entrypoint when Photon source changes
  user-main
  #?(:cljs (:require-macros user-main))
  (:require contrib.uri ; data_readers
            contrib.ednish
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            hyperfiddle.rcf
            [hyperfiddle.router :as router]
            #?(:cljs [hyperfiddle.router-html5 :as html5])
            user.demo-entrypoint

            user.demo-1-hello-world
            user.demo-2-toggle
            user.demo-3-system-properties
            user.demo-4-chat
            user.demo-4-chat-extended
            user.demo-4-webview
            user.demo-5-todomvc
            user.demo-todomvc-composed
            user.demo-6-two-clocks
            user.demo-explorer2
            user.demo-10k-dom-elements
            user.todos-simple
            user.seven-gui-1-counter
            user.seven-gui-2-temperature-converter
            user.seven-gui-4-timer
            user.seven-gui-5-crud
            user.demo-scrollview
            user.demo-color
            user.tic-tac-toe
            wip.demo-branched-route
            #_wip.hfql
            wip.tag-picker
            wip.teeshirt-orders

            ; these demos require extra deps alias
            #_wip.dennis-exception-leak
            #_wip.demo-stage-ui4
            #_wip.datomic-browser
            ))

#?(:cljs (defn set-page-title! [route]
           (set! (.-title js/document)
                 (str (clojure.string/capitalize (name (first route))) " - Hyperfiddle"))))

(p/defn NotFoundPage []
  (p/client (dom/h1 (dom/text "Page not found"))))

(p/defn Pages [page]
  (p/server
    (case page
      :user.demo-entrypoint/Demos user.demo-entrypoint/Demos
      :user.demo-entrypoint/Secrets user.demo-entrypoint/Secrets
      :user.demo-entrypoint/hfql-teeshirt-orders wip.teeshirt-orders/App
      :user.demo-entrypoint/explorer user.demo-explorer2/App
      :user.demo-entrypoint/demo-10k-dom-elements user.demo-10k-dom-elements/App ; todo too slow to unmount, crashes
      :user.demo-entrypoint/router wip.demo-branched-route/App
      :user.demo-entrypoint/tag-picker wip.tag-picker/App
      :user.demo-entrypoint/hello-world user.demo-1-hello-world/App
      :user.demo-entrypoint/toggle user.demo-2-toggle/App
      :user.demo-entrypoint/system-properties user.demo-3-system-properties/App
      :user.demo-entrypoint/chat user.demo-4-chat/App
      :user.demo-entrypoint/chat-extended user.demo-4-chat-extended/App
      :user.demo-entrypoint/webview user.demo-4-webview/App
      :user.demo-entrypoint/todos-simple user.todos-simple/Todo-list ; css fixes
      :user.demo-entrypoint/todomvc user.demo-5-todomvc/App
      :user.demo-entrypoint/todomvc-composed user.demo-todomvc-composed/App
      :user.demo-entrypoint/color user.demo-color/App
      :user.demo-entrypoint/two-clocks user.demo-6-two-clocks/App
      :user.demo-entrypoint/infinite-scroll user.demo-scrollview/Demo
      :user.demo-entrypoint/seven-guis-counter user.seven-gui-1-counter/Counter
      :user.demo-entrypoint/seven-guis-temperature-converter user.seven-gui-2-temperature-converter/App
      :user.demo-entrypoint/seven-guis-timer user.seven-gui-4-timer/Timer
      :user.demo-entrypoint/seven-guis-crud user.seven-gui-5-crud/App
      :user.demo-entrypoint/tic-tac-toe user.tic-tac-toe/App
      ;:user.demo-entrypoint/dennis-exception-leak wip.dennis-exception-leak/App2
      ;:user.demo-entrypoint/demo-stage-ui4 wip.demo-stage-ui4/Demo
      ;:user.demo-entrypoint/datomic-browser wip.datomic-browser/App
      NotFoundPage)))

(p/defn Main []
  (binding [router/encode contrib.ednish/encode-uri
            router/decode #(or (contrib.ednish/decode-path % hf/read-edn-str)
                               [:user.demo-entrypoint/Demos]
                               #_[[:user.demo-entrypoint/Demos . . .]]
                               #_{:user.demo-entrypoint/Demos {0 . 1 . 2 .}})]
    (router/router (html5/HTML5-History.)
      (set-page-title! router/route)
      (binding [dom/node js/document.body]
        (dom/pre (dom/text (contrib.str/pprint-str router/route)))
        (let [[page & args] router/route]
          ; set up detached sub app with its own state
          ; routing is still via qualified name in sub apps, so they can route home for example
          (router/router 1 ; sub-app can't know its own "react key"
            (p/server (new (Pages. page) #_args))))))))
