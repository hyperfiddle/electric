(ns wip.demo-branched-route
  (:require datascript.core
            dev
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.api :as hf]
            wip.orders-datascript
            [contrib.ednish :as ednish]
            [hyperfiddle.router :as router]
            [hyperfiddle.popover-ui2]
            [hyperfiddle.hfql.tree-to-grid-ui :as ttgui]
            wip.orders-datascript)
  #?(:cljs (:require-macros [wip.demo-branched-route])))

(p/def Page)
(p/defn Page-impl []
  (dom/h1 (dom/text "Branched route"))
  (p/server
    (binding [hf/*nav!*   wip.orders-datascript/nav!
              hf/*schema* wip.orders-datascript/schema
              hf/db       hf/*$*]
      (ttgui/with-gridsheet-renderer
        (binding [ttgui/grid-width 2
                  hf/db-name "$"]
          (p/server
            (let [order 9]
              (binding [hf/entity order]
                (hf/hfql {(wip.orders-datascript/orders .) [:db/id]}))))))))
  (dom/hr)
  (hyperfiddle.popover-ui2/popover ::left "Recur Left" (Page.))
  (hyperfiddle.popover-ui2/popover ::right "Recur Right" (Page.)))

(defn html5-navigate! [!path route]
  #?(:cljs (if-some [route (hf/simplify-route route)]
             (do (router/pushState! !path (ednish/encode-uri route))
                 (when-some [title (if (qualified-ident? route) route (::hf/route route))]
                   (set! js/document.title (pr-str title))))
             (router/pushState! !path "/"))))

(defn html5-replace-state! [!path route]
  #?(:cljs (router/replaceState! !path (if-some [route (hf/simplify-route route)]
                                         (ednish/encode-uri route)
                                         "/"))))

(p/defn App []
  (hf/branch
    (p/client
      (hf/router
        (p/fn [!path] (ednish/decode-path (router/path !path) hf/read-edn-str))
        html5-navigate!
        #(.back js/window.history)
        html5-replace-state!
        (binding [Page Page-impl]
          (Page.))))))

