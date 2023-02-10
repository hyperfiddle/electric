(ns wip.demo-branched-route
  #?(:cljs (:require-macros [wip.demo-branched-route]))
  (:require datascript.core
            dev
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql.tree-to-grid-ui :as ttgui]
            [hyperfiddle.electric :as p]
            [hyperfiddle.electric-dom2 :as dom]
            hyperfiddle.popover
            [hyperfiddle.router :as router]
            wip.orders-datascript))

(p/def Page)
(p/defn Page-impl []
  (dom/h1 (dom/text "Branched route"))
  (dom/pre (dom/text (contrib.str/pprint-str router/route)))

  (router/router :hfql
    (p/server
      (binding [hf/*nav!* wip.orders-datascript/nav!
                hf/*schema* wip.orders-datascript/schema
                hf/db hf/*$*]
        (ttgui/with-gridsheet-renderer
          (binding [ttgui/grid-width 2
                    hf/db-name "$"]
            (p/server
              (hf/hfql {(wip.orders-datascript/orders .) [:db/id]})))))))

  (dom/hr)
  (router/router :left (hyperfiddle.popover/popover "Recur Left" (Page.)))
  (router/router :right (hyperfiddle.popover/popover "Recur Right" (Page.))))

(p/defn App []
  (hf/branch
    (p/client
      (binding [Page Page-impl]
        (router/router 1 ; ordinal to nominal - representation only
          (Page.))))))

(comment
  (p/for [[page nested] s]
    (router/router page (hyperfiddle.popover/popover "Recur Left"
                          (hf/eval-as-iframe nested))))

  `(wip.demo-branched-route/App
     {::needle "root"
      ::left {::needle ""
              ::left {::needle ""
                      ::right {}}}
      ::right {::needle ""}})

  `(wip.demo-branched-route/App
     {::left `(wip.demo-branched-route/PDF)
      ::right `(wip.demo-branched-route/HTML)})
  )