(ns user.demo-2-toggle
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  #?(:cljs (:require-macros user.demo-2-toggle)))

(defonce !x #?(:clj (atom true) :cljs nil)) ; server
(p/def x (p/server (p/watch !x)))

(p/defn App []
  (p/client
    (dom/h1 "Toggle")
    (ui/button {::ui/click-event (p/fn [e]
                                   (p/server (swap! !x not)))}
      "toggle client/server")
    (dom/p
      "Number type is: "
      (if (p/server x)
        (p/client (pr-str (type 1)))           ; javascript number type
        (p/server (pr-str (type 1)))))))       ; java number type
