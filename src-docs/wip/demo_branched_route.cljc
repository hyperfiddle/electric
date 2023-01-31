(ns wip.demo-branched-route
  #?(:cljs (:require-macros [wip.demo-branched-route]))
  (:require datascript.core
            dev
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql.tree-to-grid-ui :as ttgui]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            hyperfiddle.popover
            [hyperfiddle.router :as router]
            wip.orders-datascript))

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
            (hf/hfql {(wip.orders-datascript/orders .) [:db/id]}))))))
  (dom/hr)
  (router/router ::left  (hyperfiddle.popover/popover "Recur Left" (Page.)))
  (router/router ::right (hyperfiddle.popover/popover "Recur Right" (Page.))))

(p/defn App []
  (hf/branch
    (p/client
      (binding [Page Page-impl]
        (Page.)))))
