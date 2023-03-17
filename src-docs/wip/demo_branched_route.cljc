(ns wip.demo-branched-route
  (:require datascript.core
            #?(:clj user.example-datascript-db)
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql-tree-grid :as ttgui]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            hyperfiddle.popover
            [hyperfiddle.history :as router]
            wip.orders-datascript))

(e/def Page)
(e/defn Page-impl []
  (dom/h1 (dom/text "Branched route"))
  (dom/pre (dom/text (contrib.str/pprint-str router/route)))

  (router/router :hfql
    (e/server
      (binding [hf/*nav!* wip.orders-datascript/nav!
                hf/*schema* wip.orders-datascript/schema
                hf/db hf/*$*]
        (ttgui/with-gridsheet-renderer
          (binding [ttgui/grid-width 2
                    hf/db-name "$"]
            (e/server
              (hf/hfql {(wip.orders-datascript/orders .) [:db/id]})))))))

  (dom/hr)
  (router/router :left (hyperfiddle.popover/popover "Recur Left" (Page.)))
  (router/router :right (hyperfiddle.popover/popover "Recur Right" (Page.))))

(e/defn RecursiveRouter []
  (hf/branch
    (e/client
      (binding [Page Page-impl]
        (router/router 1 ; ordinal to nominal - representation only
          (Page.))))))

(comment
  (e/for [[page nested] s]
    (router/router page (hyperfiddle.popover/popover "Recur Left"
                          (hf/eval-as-iframe nested))))

  `(wip.demo-branched-route/RecursiveRouter
     {::needle "root"
      ::left {::needle ""
              ::left {::needle ""
                      ::right {}}}
      ::right {::needle ""}})

  `(wip.demo-branched-route/RecursiveRouter
     {::left `(wip.demo-branched-route/PDF)
      ::right `(wip.demo-branched-route/HTML)})
  )