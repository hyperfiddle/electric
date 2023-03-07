(ns user.demo-virtual-scroll
  (:require [contrib.data :refer [unqualify]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            #?(:cljs goog.object)))

(e/defn DemoFixedHeightCounted
  "Scrolls like google sheets. this can efficiently jump through a large indexed collection"
  []
  (let [row-count 500
        xs (vec (range row-count)) ; counted
        page-size 100
        row-height 22] ; todo use relative measurement (browser zoom impacts px height)
    (e/client
      (dom/div (dom/props {:class "viewport" :style {:overflowX "hidden" :overflowY "auto"}})
        (let [[scrollTop] (new (ui/scroll-state< dom/node))
              max-height (* row-count row-height)
              clamped-scroll-top (js/Math.min scrollTop max-height)
              start (/ clamped-scroll-top row-height)] ; (js/Math.floor)
          (dom/div (dom/props {:style {:height (str (* row-height row-count) "px") ; optional absolute scrollbar
                                       :padding-top (str clamped-scroll-top "px") ; seen elements are replaced with padding
                                       :padding-bottom (str (- max-height clamped-scroll-top) "px")}})
            (e/server
              ; seen elements are unmounted
              (e/for [x #_(subvec xs
                            (Math/min start row-count)
                            (Math/min (+ start page-size) row-count))
                      (->> xs (drop start) (take page-size))]
                (e/client (dom/div (dom/text x)))))))))))

(e/defn DemoVariableHeightInfinite
  "Scrolls like newsfeed. Natural browser layout for variable height rows. Leaves seen elements
  mounted in the DOM."
  []
  (let [xs (range) ; infinite
        page-size 100]
    (e/client
      (dom/div (dom/props {:class "viewport"})
        (let [!pages (atom 1) pages (e/watch !pages)
              [scrollTop scrollHeight clientHeight] (new (ui/scroll-state< dom/node))]
          (when (>= scrollTop (- scrollHeight clientHeight clientHeight)) ; scrollThresholdPx = clientHeight
            (swap! !pages inc)) ; can this get spammed by Electric?
          (dom/div ; content is unstyled, uses natural layout
            (e/server
              (e/for [x (->> xs (take (* pages page-size)))] ; leave dom
                (e/client (dom/div (dom/text x)))))))))))

#?(:clj (defonce !demo (atom {:text "DemoFixedHeightCounted" ::value `DemoFixedHeightCounted})))

(e/def demo (e/server (e/watch !demo)))

(e/def demos {`DemoVariableHeightInfinite DemoVariableHeightInfinite
              `DemoFixedHeightCounted DemoFixedHeightCounted})

(e/defn VirtualScroll []
  (e/client
    ; Requires css {box-sizing: border-box;}
    (dom/element "style" (dom/text ".header { position: fixed; z-index:1; top: 0; left: 0; right: 0; height: 100px; background-color: #abcdef; }"
                           ".footer { position: fixed; bottom: 0; left: 0; right: 0; height: 100px; background-color: #abcdef; }"
                           ".viewport { position: fixed; top: 100px; bottom: 100px; left: 0; right: 0; background-color: #F63; overflow: auto; }"))
    (dom/div (dom/props {:class "header"})
      (dom/dl
        (dom/dt (dom/text "scroll debug state"))
        (dom/dd (dom/pre (dom/text (pr-str (update-keys (e/watch ui/!scrollStateDebug) unqualify))))))
      (e/server
        (ui/select
          demo
          (e/fn V! [v] (reset! !demo v))
          (e/fn Options [] [{:text "DemoFixedHeightCounted" ::value `DemoFixedHeightCounted}
                            {:text "DemoVariableHeightInfinite" ::value `DemoVariableHeightInfinite}])
          (e/fn OptionLabel [x] (:text x)))))
    (e/server (new (get demos (::value demo))))
    (dom/div (dom/props {:class "footer"})
      (dom/text "Try scrolling to the top, and resizing the window."))))
