(ns user.demo-5-button
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-5-button)))


(def !n #?(:clj (atom 0)))
(p/def n (p/watch !n))                                      ; server

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Button with server callback"))
    (dom/dl
      (dom/dt (dom/text "client button"))
      (dom/dd
        (ui/button {::ui/click-event (p/fn [event]
                                       (js/console.log ::clicked event)
                                       ~@(swap! !n inc))}   ; client/server transfer
          (dom/text "click me")))
      (dom/dt (dom/text "server atom"))
      (dom/dd (dom/text ~@n))                               ; client/server transfer

      (let [odd ~@(odd? n)]                                 ; client/server transfer
        (dom/dt (dom/text (if odd "client" "server")))
        (dom/dd (dom/text (if odd
                            (pr-str (type ~@n))             ; client/server transfer
                            ~@(pr-str (type n)))))))))      ; client/server transfer

(def main #?(:cljs (p/client (p/main
                               (try
                                 (binding [dom/node (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _))))))

(comment
  (user/browser-main! `main)
  (reset! !n 0)
  )
