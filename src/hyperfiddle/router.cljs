(ns hyperfiddle.router
  (:require [contrib.missionary-contrib :refer [poll-task]]
            [hyperfiddle.rcf :as rcf :refer [% tests with tap]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [missionary.core :as m])
  (:import [missionary Cancelled])
  (:require-macros [hyperfiddle.router :refer [path]]))

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

; Public interface requirements of Rosie
; - ability to directly set the route (pushState and replaceState) through an Edn editor
; - ability to encode/decode a route value to a path string
; - ability to render links from route value (no knowledge of path encoding)

(p/def Link) ; bind this to result of ->Link
(p/defn ->Link [!path encode]
  (p/fn [route label]
    (p/client
      (let [path (encode route)]
        (ui/element dom/a {::dom/href path ; middle click
                           ::ui/click-event (p/fn [e]
                                              (.preventDefault e)
                                              (pushState! !path path))} label)))))

; Demo

(p/defn PathRouterDemo [path]
  (case path
    "/" (do (dom/h1 "Home") (Link. "/a" "a"))
    "/a" (do (dom/h1 "A") (Link. "/" "home"))
    (do (dom/h1 "404, route: " (pr-str path)) (Link. "/" "home"))))

(p/defn NamedRouterDemo [route]
  (case route
    ::home (do (dom/h1 "Home") (Link. ::a "a"))
    ::a (do (dom/h1 "A") (Link. ::home "home"))
    (do (dom/h1 "404, route: " (pr-str route)) (Link. "/" "home"))))

(p/defn Demo []
  (let [!path (m/mbx), path (path !path)]
    (let [encode-path identity
          decode-path identity]
      (binding [Link (->Link. !path encode-path)]
        (PathRouterDemo. (decode-path path))))
    (let [decode-path {"/" ::home "/a" ::a}
          encode-path (clojure.set/map-invert decode-path)]
      (binding [Link (->Link. !path encode-path)]
        (NamedRouterDemo. (decode-path path))))))
