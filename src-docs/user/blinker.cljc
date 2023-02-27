(ns user.blinker
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

(defn query [] "blink!")

(e/defn MyComponent []
  (dom/h1 (dom/text (e/server (query))))
  (e/on-mount #(println 'component-did-mount))
  (e/on-unmount #(println 'component-will-unmount)))

(e/defn Blinker []
  (e/client
    (dom/h1 (dom/text "Blinker"))
    (if (= 0 (int (mod e/system-time-secs 2)))
      (MyComponent.)))) ; added and removed from the DOM every two seconds
