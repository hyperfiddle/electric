(ns user.router
  (:require [contrib.missionary-contrib :refer [poll-task]]
            [hyperfiddle.rcf :as rcf :refer [% tests with tap]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [missionary.core :as m])
  (:import [missionary Cancelled]))

;(p/def !path (m/mbx)) -- fixme, photon bug https://www.notion.so/hyperfiddle/p-def-in-cljs-file-does-not-register-photon-node-meta-on-the-cljs-var-there-is-no-such-var-on-the--da302349e1ed4c6d9e0cd6e32dfd472c
(p/def routes {})

(defn pushState!
  ;([path] (pushState! !path path))
  ([!path path] (.pushState js/window.history nil "" path) (!path path)))

(defn replaceState!
  ;([path] (replaceState! !path path))
  ([!path path] (.replaceState js/window.history nil "" path) (!path path)))

(def >location "discrete flow of current window location"
  (m/observe (fn [!]
               ; popstate is back/forward event, platform has already set the new location
               (let [f (fn [e] (! (-> js/window .-location)))]
                 (f nil)
                 (.addEventListener js/window "popstate" f)
                 #(.removeEventListener js/window "popstate" f)))))

; equivalent formulation but with more flow machinery
#_
(def >location
  (m/ap (m/amb= (-> js/window .-location)
                (do (m/?> (m/observe (fn [!]
                                       (.addEventListener js/window "popstate" !) ; popstate is back/forward event
                                       #(.removeEventListener js/window "popstate" !))))
                    (-> js/window .-location))))) ; platform has already set the new location

(def loc->token (juxt #(.-pathname %) #(.-search %) #(.-hash %)))
(defn loc->path [^js location] (apply str (loc->token location)))

(defn path> "discrete flow of current window location as string, e.g. \"/a?b#c\"."
  ; browser differentiates between physical navigation and js navigation, it only calls popstate
  ; on physical navigation. So we must simulate this event with a mailbox. Note goog.history
  ; has the same mailbox machinery.
  ;([] (path> !path))
  ([!path] (path> !path loc->path))
  ([!path loc->path]
   (m/ap (m/amb= (loc->path (m/?> >location)) ; connect back/forward browser events to the router
                 (m/?> (poll-task !path))))))

(tests
  (def !path (m/mbx))
  (def >path (path> !path))

  (def it (>path #(tap ::notify) #(tap ::terminate)))
  % := ::notify @it := _
  (replaceState! !path "/")   % := ::notify @it := "/"
  (pushState! !path "/a")     % := ::notify @it := "/a"
  (pushState! !path "/a?b")   % := ::notify @it := "/a?b"
  (pushState! !path "/a?b#c") % := ::notify @it := "/a?b#c"

  "back/forward"
  ; the effect is async, wait for notify before sample
  (.back js/window.history)    % := ::notify @it := "/a?b"
  (.forward js/window.history) % := ::notify @it := "/a?b#c"

  ; must sample terminal value to cause m/observe to unmount - due to backpressure
  (it) % := ::notify @it :throws missionary.Cancelled) ; Mailbox fetch cancelled.

; Demo

(p/defn Link [!path route label] ; todo !path should be dynamic - photon bug
  (let [href (get (clojure.set/map-invert routes) route route)] ; backwards compat
    (ui/element dom/a {::dom/href href
                       ::ui/click-event (p/fn [e]
                                          (pushState! !path href)
                                          (.preventDefault e))} label)))
;(defn -resolve-dynamic [] #'!path)

(p/defn App []
  ;(println `path !path (meta (-resolve-dynamic)))
  ;(binding [!path (m/mbx)])
  (let [!path (m/mbx)
        path (new (m/relieve {} (path> !path)))]
    (PathRouterDemo. !path path)
    (NamedRouterDemo. !path path)))

(p/defn PathRouterDemo [!path path]
  (case path
    "/" (do (dom/h1 "Home") (Link. !path "/a" "a"))
    "/a" (do (dom/h1 "A") (Link. !path "/" "home"))
    (do (dom/h1 "404, route: " (pr-str path)) (Link. !path "/" "home"))))

(p/defn NamedRouterDemo [!path path]
  (binding [routes {"/" ::home "/a" ::a}] ; optional
    (case (routes path)
      ::home (do (dom/h1 "Home") (Link. !path ::a "a"))
      ::a (do (dom/h1 "A") (Link. !path ::home "home"))
      (do (dom/h1 "404, route: " (pr-str path)) (Link. !path "/" "home")))))
