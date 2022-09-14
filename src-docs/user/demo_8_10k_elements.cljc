(ns user.demo-8-10k-elements
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.ui.color :refer [rgb hsv->rgb]]
            [missionary.core :as m])
  #?(:cljs (:require-macros user.demo-8-10k-elements)))

(def !moves #?(:clj (atom []) :cljs nil))
(def !board-size #?(:clj (atom 10000) :cljs nil))
(p/def board-size (p/server (p/watch !board-size)))

(comment (do (reset! !moves []) (reset! !board-size 2000)))

;(defn countdown [x]                     ; Count down to 0 then terminate.
;  (m/relieve {} (m/ap (loop [x x]
;                        (m/amb x
;                          (if (pos? x)
;                            (do (m/? (m/sleep 100))
;                                (recur (dec x)))
;                            x))))))
;
;(defn cell-color [x]
;  (if (> x 1) ; In photon-land, this conditional would introduce a switch and trigger a ws message for client-server frame coordination.
;    (apply rgb (hsv->rgb (/ 0 360)
;                 (-> x (/ 7.5) (* 1.33))
;                 0.95))
;    "#eee"))
;
;#?(:cljs (defn x [!el]
;           (m/observe (fn mount [!]
;                        (let [!o (js/MutationObserver !)]
;                          (.observe !o !el #js {"attributes" true})
;                          (fn unmount [] (.disconnect !o)))))))

#?(:cljs
   (defn hot [el]
     (m/observe (fn mount [!]
                  (dom/set-property! el "style" {:background-color "red"})
                  (! nil) ; initial value
                  (fn unmount []
                    (dom/set-property! el "style" {:background-color nil}))))))

(p/defn App []
  (p/client
    (dom/h1 "10k dom elements (multiplayer)")
    ; fixed width font + inline-block optimizes browser layout
    (dom/element "style" ".board div { width: 1em; height: 1em; display: inline-block; border: 1px #eee solid; }")
    (dom/element "style" ".board { font-family: monospace; font-size: 7px; margin: 0; padding: 0; line-height: 0; }")
    (dom/div {:class "board"}
      (p/for [i (range 0 board-size)]
        (ui/element dom/div {::ui/mouseover-event (p/fn [e] (p/server (swap! !moves conj i)))}))

      (p/for [i (p/server (p/watch !moves))]
        ; differential side effects, indexed by HTMLCollection
        (new (hot (.item (.. dom/node -children) i)))))))
