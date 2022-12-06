(ns .
  (:require
    [contrib.str :refer [pprint-str]]
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom :as dom]
    [hyperfiddle.photon-ui2 :as ui])
  #?(:cljs (:require-macros .))
  (:import (hyperfiddle.photon Pending)))

(p/defn F [e] (p/server (d/transact! conn [(tx-from-db db)])))

(button {::click-event F}
        "toggle client/server 4")

(when-some [e (button "toggle client/server 4")]
  (F. e))

(p/with-cycle [busy# IDLE]
  (try
    (when-some [e (button "toggle client/server 4" busy#)]
      (F. e))
    IDLE (catch Pending _ BUSY)))


(button {::click-event (p/fn [e] (p/server (swap! !x not)))}
        "toggle client/server 4")

(when-some [e (button "toggle client/server 4")]
  (p/server (swap! !x not)))

; see user.demo-2-toggle for demos
