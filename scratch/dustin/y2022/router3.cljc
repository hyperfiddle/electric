(ns dustin.y2022.router3
  (:require #?(:cljs goog.events)
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m])
  #?(:cljs (:import goog.history.Html5History
                    goog.history.Html5History.TokenTransformer
                    goog.history.EventType))
  #?(:cljs (:require-macros dustin.y2022.router3)))

#?(:cljs
   (defn new-history
     ([] (new-history (fn [path-prefix location]
                        (str (.-pathname location)
                             (.-search location)
                             (.-hash location)))))
     ([retrieve-fn]
      (let [transformer (TokenTransformer.)]
        (set! (.. transformer -retrieveToken) retrieve-fn)
        (set! (.. transformer -createUrl) (fn [token path-prefix location] (str path-prefix token)))
        (doto (Html5History. js/window transformer)
          (.setUseFragment false)
          (.setPathPrefix "")
          (.setEnabled true))))))

#?(:cljs
   (defn history> [^js h]
     (m/observe (fn mount [!]
                  (let [k (goog.events/listen h EventType.NAVIGATE (fn [^js e] (! (.-token e))))]
                    (! (.getToken h))
                    (fn unmount []
                      (goog.events/unlistenByKey k)))))))

#?(:cljs
   (tests
     (type 1)
     (.isSupported Html5History js/window) := true
     (def h (new-history))
     (def >x (m/cp (tap (m/?< (m/relieve {} (history> h))))))
     (def it (>x #(tap ::notify) #(tap ::terminate)))
     % := ::notify
     @it
     % := "/"
     (.setToken h "/a")
     % := ::notify
     @it
     % := "/a"
     (.setToken h "/b")
     % := ::notify
     (.setToken h "/")
     @it
     % := "/"
     (it)))

(p/def history)

(p/defn Link [href label]
  (ui/element dom/a {::dom/href href
                     ::ui/click-event (p/fn [e] (.setToken history href) (.preventDefault e))}
    label))

(p/defn App []
  (p/client
    (binding [history (new-history)]
      (let [route (new (m/relieve {} (history> history)))]
        (case route
          "/" (do (dom/h1 "Home") (Link. "/a" "a"))
          "/a" (do (dom/h1 "A") (Link. "/" "home"))
          (dom/h1 "no matching route: " (pr-str route)))))))
