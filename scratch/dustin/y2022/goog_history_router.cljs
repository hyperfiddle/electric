(ns dustin.y2022.goog-history-router
  (:require goog.events
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m])
  (:import goog.history.Html5History
           goog.history.Html5History.TokenTransformer
           goog.history.EventType))

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
       (.setEnabled true)))))

(defn path> [^js h]
  (m/observe (fn mount [!]
               (let [k (goog.events/listen h EventType.NAVIGATE (fn [^js e] (! (.-token e))))]
                 (! (.getToken h))
                 (fn unmount []
                   (goog.events/unlistenByKey k))))))

(tests
  (type 1)
  (.isSupported Html5History js/window) := true
  (def h (new-history))
  (def >x (m/cp (tap (m/?< (m/relieve {} (path> h))))))
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
  (it))

(p/def !path)

(p/defn Link [href label]
  (ui/element dom/a {::dom/href href
                     ::ui/click-event (p/fn [e]
                                        (.setToken !path href)
                                        (.preventDefault e))} label))

(p/defn App []
  (binding [!path (new-history)]
    (let [path (new (m/relieve {} (path> !path)))
          route (identity path)]
      (case route
        "/" (do (dom/h1 "Home") (Link. "/a" "a"))
        "/a" (do (dom/h1 "A") (Link. "/" "home"))
        (dom/h1 "no matching route: " (pr-str path))))))
