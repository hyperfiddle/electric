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
  (router/router ::left  (hyperfiddle.popover-ui2/popover "Recur Left" (Page.)))
  (router/router ::right (hyperfiddle.popover-ui2/popover "Recur Right" (Page.))))

(p/defn App []
  (hf/branch
    (p/client
      (binding [Page Page-impl]
        (Page.)))))

