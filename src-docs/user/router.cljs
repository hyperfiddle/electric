(ns user.router
  (:require [hyperfiddle.rcf :as rcf :refer [% tests with tap]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [missionary.core :as m])
  (:import [missionary Cancelled]))

(defn mbx->flow [mbx]
  (m/ap
    (loop [v (m/? mbx)]
      (m/amb v (recur (m/? mbx))))))

(defn pushState! [!route path] (.pushState js/window.history nil "" path) (!route path))
(defn replaceState! [!route path] (.replaceState js/window.history nil "" path) (!route path))

(defn path> "discrete flow of location pathname"
  ; browser differentiates between physical navigation and js navigation, it only calls popstate
  ; on physical navigation. So we must simulate this event with a mailbox. Note goog.history
  ; has the same mailbox machinery.
  ([mbx] (path> mbx (fn [e] (let [loc (-> js/window .-location)]
                              (str (.-pathname loc) (.-search loc) (.-hash loc))))))
  ([mbx current-path!]
   (m/ap
     (m/amb=
       ; process to connect back/forward browser events to the router
       (m/?> (m/observe (fn mount [!]
                          #_(println ::mount)
                          (! (current-path!))
                          (let [f (fn [e] (! (current-path!)))]
                            ; popstate is back/forward event, platform has already set the new location
                            (.addEventListener js/window "popstate" f)
                            (fn unmount []
                              #_(println ::unmount)
                              (.removeEventListener js/window "popstate" f))))))
       ; userland route events, because platform pushState and replaceState do not trigger popstate
       (m/?> (mbx->flow mbx))))))

(tests
  ; careful, only passes from a clean page due to (?)
  (def !route (m/mbx))
  (def route> (path> !route))

  (def it (route> #(tap ::notify) #(tap ::terminate)))
  % := ::notify @it := _
  (replaceState! !route "/")   % := ::notify @it := "/"
  (pushState! !route "/a")     % := ::notify @it := "/a"
  (pushState! !route "/a?b")   % := ::notify @it := "/a?b"
  (pushState! !route "/a?b#c") % := ::notify @it := "/a?b#c"

  "back/forward"
  ; the effect is async, wait for notify before sample
  (.back js/window.history)    % := ::notify @it := "/a?b"
  (.forward js/window.history) % := ::notify @it := "/a?b#c"

  ; must sample terminal value to cause m/observe to unmount - due to backpressure
  (it) % := ::notify @it :throws missionary.Cancelled) ; Mailbox fetch cancelled.

; Demo

(p/def !route)
(p/def routes {})

(p/defn Link [route label]
  (let [href (get (clojure.set/map-invert routes) route route)] ; backwards compat
    (ui/element dom/a {::dom/href href
                       ::ui/click-event (p/fn [e]
                                          (pushState! !route href)
                                          (.preventDefault e))} label)))

(p/defn App []
  (binding [!route (m/mbx)]
    (let [route (new (m/relieve {} (path> !route #_js/window)))]
      (case route
        "/" (do (dom/h1 "Home") (Link. "/a" "a"))
        "/a" (do (dom/h1 "A") (Link. "/" "home"))
        (dom/h1 "404 no matching route: " (pr-str route))))))

(p/defn App2 []
  (binding [!route (m/mbx)
            routes {"/" ::home "/b" ::b}] ; optional
    (let [route (new (m/relieve {} (path> !route #_js/window)))]
      (case (routes route)
        ::home (do (dom/h1 "Home") (Link. ::b "b"))
        ::b (do (dom/h1 "B") (Link. ::home "home"))
        (dom/h1 "404 no matching route: " (pr-str route))))))

; missing route? doesn't matter, let userland handle it
