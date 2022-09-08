(ns hyperfiddle.chatview
  (:require [contrib.data :refer [qualify auto-props]]
            [clojure.spec.alpha :as s]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [user.util :refer [paginate]]
            #?(:cljs goog.object))
  #?(:cljs (:require-macros hyperfiddle.chatview)))

; https://github.com/jsdevkr/react-chatview/blob/master/src/react-chatview.js

#?(:cljs
   (defn isPassedThreshold [scrollable flipped scrollLoadThresholdPx]
     ; Fractional pixel heights, like caused by browser zoom, can cause rounding errors
     ; which can misalign this
     (if flipped
       (<= (.. scrollable -scrollTop) scrollLoadThresholdPx)
       (>= (.. scrollable -scrollTop) (- (.. scrollable -scrollHeight)
                                         (.. scrollable -clientHeight)
                                         scrollLoadThresholdPx)))))

#?(:cljs (def !scrollState
           (atom {:scrollTop 0 ; optimization - detect changes (pointless)
                  :scrollHeight 0}))) ; snapshot height to detect layout shifts in flipped mode

#?(:cljs
   (defn onScroll [scrollable load-more! flipped scrollLoadThresholdPx]
     {:post [(some? %)]}
     ; detect when dom has changed underneath us - either scrollTop or scrollHeight
     ; (layout reflow) may have changed.
     (let [{:keys [scrollTop scrollHeight]} @!scrollState
           scrollTop' (.. scrollable -scrollTop)
           scrollHeight' (.. scrollable -scrollHeight)]

       (when (not= scrollTop' scrollTop) ; why guard, of course it changed

         (when (and (some? load-more!) (isPassedThreshold scrollable flipped scrollLoadThresholdPx))
           ; can this get spammed by photon?
           (load-more! 1))

         ; Detect if items were removed from the list, and write the scrollTop to stabilize the UX
         (let [scrollTop'' (+ scrollTop'
                              (if flipped (- scrollHeight' scrollHeight) 0))

               ; when scrollHeightDifference is > 0, something was removed from list
               scrollHeightDifference (if scrollHeight (- scrollHeight scrollHeight') 0)

               ; in flipped mode, the scrollTop is sensitive to layout shifts above us, such as
               ; element removal. (In normal mode, such layout shifts are below and do not impact
               ; scrollTop.) Override the scrollTop to correct for this jank.
               scrollTop'' (if (and flipped (> scrollHeightDifference 0))
                             (+ scrollTop'' scrollHeightDifference)
                             scrollTop'')]

           (if (not= scrollTop'' scrollTop')
             (set! (.. scrollable -scrollTop) scrollTop''))

           ; cycle - changes the padding which changes the scroll
           (reset! !scrollState {:scrollTop scrollTop''
                                 ; re-sample scrollHeight? for what purpose
                                 :scrollHeight (.. scrollable -scrollHeight)})
           (load-more! scrollTop''))))))

#?(:cljs (defn onScroll< [scrollable
                          {:keys [::flipped ::scrollLoadThresholdPx]
                           :or {flipped false scrollLoadThresholdPx 100}}]
           (->> (m/observe
                  (fn mount [!]
                    (println `mount)
                    (let [heightDifference (if flipped (- (.. scrollable -scrollHeight)
                                                          (.. scrollable -clientHeight)) 0)
                          opts #js {"passive" true}
                          f (partial onScroll scrollable ! flipped scrollLoadThresholdPx)]
                      ;(set! (.. scrollable -scrollTop) heightDifference) ; todo repeated
                      (.addEventListener scrollable "scroll" f opts)
                      (fn unmount []
                        (println `unmount)
                        (.removeEventListener scrollable "scroll" f opts)))))
                (m/reductions {} 0)
                (m/latest identity))))

(p/defn Demo []
  (dom/element "style" (str ".header { position: fixed; top: 0; left: 0; right: 0; height: 100px; background-color: #abcdef; }"
                            ".footer { position: fixed; bottom: 0; left: 0; right: 0; height: 100px; background-color: #abcdef; }"
                            ".viewport { position: fixed; top: 100px; bottom: 100px; left: 0; right: 0; background-color: #F63; overflow: auto; }"))
  (dom/div {:class "header"}
    (let [{:keys [scrollTop scrollHeight]} (p/watch !scrollStateDebug)]
      (dom/dl (dom/dt "scrollTop") (dom/dd scrollTop)
              (dom/dt "scrollHeight") (dom/dd scrollHeight))))

  ; IndexedScroll
  ; Long-jump scroll bar for efficient indexed views into a large list. Lazy seqs are only realized
  ;  as necessary. If total count is provided, projects scrollbar height based on observed heights of
  ;  seen elements. Requires css {box-sizing: border-box;}

  (p/server
    (let [rowCount 500 xs (range rowCount)
          limit 50 rowHeight 22]
      (p/client
        ; row height must be sampled due to browser zoom etc, or be in relative units
        (dom/div {:class "viewport" :style {:overflowX "hidden" :overflowY "auto"}}
          ; observe scrollTop to paginate list
          (let [scrollTop (new (onScroll< dom/node {}))
                _ (println `scrollTop scrollTop)
                start (/ scrollTop rowHeight) ; chunk this for latency efficiency
                projectedHeight (if (some? rowCount) (str (* rowCount rowHeight) "px"))]

            ; To paginate: how deep are we, in number of rows?
            ; What can we know?
            ;   pixel depth
            ;   number of rows seen so far (dropped), and their running height (scrollTop)
            ;      rows that were dropped / scrollTop
            ;   projected height


            (dom/div ; content
              {:style {:height projectedHeight
                       :padding-top (str scrollTop "px")}}
              (p/server
                (p/for [x (->> xs (drop start) (take limit))]
                  (p/client (dom/div x))))))))))

  ; Seq scroll
  ; Leaves loaded items in DOM, which is good for variable height rows and for local viewing of
  ;  recent messages
  #_
  (p/server
    (let [n 500
          xs (range n)]
      (p/client
        (dom/div {:class "viewport"}
          (let [!pages (atom 1) pages (p/watch !pages)]
            (new (onScroll< dom/node (partial swap! !pages inc) {}))
            (dom/div ; content
              (p/server
                #_(if (and flipped (not reversed)) (reverse lazy-children) lazy-children)
                (let [xs (take (* pages 60) xs)]
                  (p/for [x xs]
                    (p/client (dom/div x)))))))))))
  (dom/div {:class "footer"} "Try scrolling to the top, and resizing the window."))
