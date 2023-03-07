(ns user.demo-10k-dom
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]))

(def !moves #?(:clj (atom []) :cljs nil))
(def !board-size #?(:clj (atom 10000) :cljs nil))
(e/def board-size (e/server (e/watch !board-size)))

(comment (do (reset! !moves []) (reset! !board-size 2000)))

#?(:cljs
   (defn hot [el]
     (m/observe (fn mount [!]
                  (dom/set-property! el "style" {:background-color "red"})
                  (! nil) ; initial value
                  (fn unmount []
                    (dom/set-property! el "style" {:background-color nil}))))))

(e/defn Dom-10k-Elements []
  (e/client
    (dom/h1 (dom/text "10k dom elements (multiplayer)"))
    ; fixed width font + inline-block optimizes browser layout
    (dom/element "style" (dom/text ".board div { width: 1em; height: 1em; display: inline-block; border: 1px #eee solid; }"))
    (dom/element "style" (dom/text ".board { font-family: monospace; font-size: 7px; margin: 0; padding: 0; line-height: 0; }"))
    (dom/div {:class "board"}
      (e/for [i (range 0 board-size)]
        (dom/div
          (dom/on "mouseover" (e/fn [e] (e/server (swap! !moves conj i))))))
      (e/for [i (e/server (e/watch !moves))]
        ; differential side effects, indexed by HTMLCollection
        (new (hot (.item (.. dom/node -children) i)))))))

;(defn countdown [x]                     ; Count down to 0 then terminate.
;  (m/relieve {} (m/ap (loop [x x]
;                        (m/amb x
;                          (if (pos? x)
;                            (do (m/? (m/sleep 100))
;                                (recur (dec x)))
;                            x))))))
;
;(defn cell-color [x]
;  (if (> x 1) ; In Electric-land, this conditional would introduce a switch and trigger a ws message for client-server frame coordination.
;    (apply css-rgb-str (hsv->rgb (/ 0 360)
;                 (-> x (/ 7.5) (* 1.33))
;                 0.95))
;    "#eee"))
;
;#?(:cljs (defn x [!el]
;           (m/observe (fn mount [!]
;                        (let [!o (js/MutationObserver !)]
;                          (.observe !o !el #js {"attributes" true})
;                          (fn unmount [] (.disconnect !o)))))))