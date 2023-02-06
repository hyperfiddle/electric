

; demo entrypoint
{:hyperfiddle.api/route [:user.demo-entrypoint/hfql-teeshirt-orders],
 :user.demo-entrypoint/hfql-teeshirt-orders
 {:wip.teeshirt-orders/route [wip.orders-datascript/one-order 10]}}

; teeshirt-orders scope
{:wip.teeshirt-orders/route [wip.orders-datascript/one-order 10]}


(comment
  {}
  {::demo-entrypoint {}}
  {::hello-world {}}
  {::system-properties {}}
  {::system-properties {::search "java"}}

  ; what's this?
  {::hello-world {}
   ::system-properties {}}

  ; the state should be able to express branching
  {:hyperfiddle.api/route [:user.demo-entrypoint/router],
   :user.demo-entrypoint/router
   {:wip.demo-branched-route/right nil,
    :wip.demo-branched-route/left nil,
    [wip.orders-datascript/orders .] {:needle "bob"}}}

  {:user.demo-entrypoint/router
   {'[wip.orders-datascript/orders .] {:needle "root"}
    :wip.demo-branched-route/left
    {'[wip.orders-datascript/orders .] {:needle "left"}}
    :wip.demo-branched-route/right
    {'[wip.orders-datascript/orders .] {:needle "right"}}}}

  {:hyperfiddle.api/route [:user.demo-entrypoint/router],
   :user.demo-entrypoint/router
   {[wip.orders-datascript/orders .] {:needle "root"}
    :wip.demo-branched-route/right
    {[wip.orders-datascript/orders .] {:needle "right"}},
    :wip.demo-branched-route/left
    {[wip.orders-datascript/orders .] {:needle "left"}}}}



  )

