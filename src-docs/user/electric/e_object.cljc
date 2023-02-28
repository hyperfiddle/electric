(ns user.electric.e-object
  (:require
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-dom2 :as dom]
    [hyperfiddle.electric-ui4 :as ui]
    [missionary.core :as m]))

; m/observe is meant to manage a resource - it lets you subscribe to a foreign reference (say a
; Clojure atom) and then release the resource when the flow terminates.
; Here is an example of managing an atom with m/observe - https://github.com/hyperfiddle/electric/blob/master/src-docs/user/missionary/missionary_tutorial_watch.cljc#L16-L24
; (This is at the Missionary level so it's very low level)
; the point is that m/observe lets you detect the flow's cancellation signal and perform a side effect in response to it (edited)
; here, I modified the toggle demo, to demonstrate using m/observe to hook the flow lifecycle from Electric:

#?(:cljs (def !x (atom true)))

(e/defn MyObject [id]
  (new (m/observe (fn mount [!]
                    (println 'mount id)
                    (! nil) ; emit initial value, or (new) would fail
                    ; because Electric reactive values can never be undefined
                    (fn unmount []
                      (println 'unmount id))))))

(e/defn App []
  (e/client
    (dom/h1 (dom/text "Object"))
    (ui/button (e/fn [] (swap! !x not))
      (dom/text "toggle"))

    (if (e/watch !x)
      (MyObject. :a)
      (MyObject. :b))))

; the electric if will switch between the two objects, unmounting one and mounting the other
; finally, in Electric we use new to join a foreign missionary flow value
; the parallels between objects and flows are on purpose, there is a very deep connection here
; you should almost always think of e/defn as defining a reactive function, and it absolutely is a function
; you can also think of e/defn as defining an object, with a resource lifecycle and state
; it is both
; this is why we capitalize electric function names (like React.js components) and call them with new