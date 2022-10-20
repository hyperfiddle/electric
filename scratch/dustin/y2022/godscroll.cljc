(ns dustin.y2022.godscroll
  (:require [contrib.data :refer [unqualify]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]
            #?(:cljs goog.object))
  #?(:cljs (:require-macros dustin.y2022.godscroll)))

(comment
  (defn f [[scrollTop scrollHeight clientHeight] ; latest
           [scrollTop' scrollHeight' clientHeight'] ; prior
           load-more!]
    {:pre [load-more!]}
    (when (>= scrollTop (- scrollHeight clientHeight scrollLoadThresholdPx))
      ; can this get spammed by photon?
      (load-more! 1)))

  (def >window
    (let [>x (m/stream! (scroll-state> scrollable))]
      (m/zip vector (m/eduction (drop 1) >x) >x)))

  (def <window (->> >window (m/reductions {} [0 0 0]) (m/latest identity)))
  (m/cp (f (m/?< <window))))

(p/defn DemoVariableHeightCounted []
  ; Long-jump scroll bar for efficient indexed views into a large list.
  ; Absolute scroll bar projected from the variable heights that we've seen so far.
  ; project the total scrollbar length based on the height of the rows we have seen
  ; ?? Lazy seqs are only realized as necessary, but the count must be known (?)

  ; What if it is not an absolute jump but a relative jump with super-linear acceleration?
  ; relative to the number of records known
  (p/server
    (let [rowCount 500
          xs (range rowCount)
          page-size 50]
      (p/client
        (dom/div {:class "viewport" :style {:overflowX "hidden" :overflowY "auto"}}
          (let [[scrollTop scrollHeight clientHeight :as s] (new (scroll-state< dom/node))
                start (/ scrollTop rowHeight)
                projectedHeight (if (some? rowCount) (str (* rowCount rowHeight) "px"))]

            ; page-size is provided
            ; we can take a page and then measure the content height - this gives mean row-height
            ; we can also remember the height of each page we've seen, and have exact padding
            ; (assuming no window resize)

            ; To paginate: how deep are we, in number of rows?
            ; What can we know?
            ;   pixel depth
            ;   number of rows seen so far (dropped), and their running height (scrollTop)
            ;      rows that were dropped / scrollTop
            ;   projected height

            (dom/div {:style {:height projectedHeight
                              :padding-top (str scrollTop "px")}}
              (p/server
                (p/for [x (->> xs (drop start) (take page-size))]
                  (p/client (dom/div x)))))))))))

(p/def Recur')
(p/defn DemoGodScroll []
  ; variable height, infinite, super-linear acceleration, absolute-feeling jumps for counted views

  ; measure the page size
  ;    add one element at a time until the scrollHeight > clientHeight : this is one page
  ; as we advance, add padding equal to the measured height that is now removed
  ; if we jump ahead,

  (p/server
    (let [rowCount 200
          xs (range rowCount)
          !seen-count (atom 1) seen-count (p/watch !seen-count)]
      (p/client
        (dom/div {:class "viewport" :style {:overflowX "hidden" :overflowY "auto"}}
          (let [scrollable dom/node
                [scrollTop scrollHeight clientHeight] (new (scroll-state< dom/node))
                #_#_start (/ scrollTop rowHeight)
                #_#_projectedHeight (if (some? rowCount) (str (* rowCount rowHeight) "px"))
                padding scrollTop
                rowHeight (doto (/ scrollHeight seen-count) (println 'row-height))
                above-count (* rowHeight padding)]

            (dom/div {:style {#_#_:height projectedHeight
                              :padding-top (str padding "px")}}
              (p/server
                (binding
                  [Recur' (p/fn [[x & xs] n]
                            (p/client
                              (dom/div x)
                              (let [[_ scrollHeight clientHeight :as s] (sample-scroll-state! scrollable)]
                                (println `s s)
                                (if (<= scrollHeight clientHeight)
                                  (p/server
                                    (when (seq xs)
                                      (Recur'. xs (inc n))))
                                  n))))]
                  (let [n (Recur'. (drop above-count xs) above-count)]
                    (reset! !seen-count n)))))))))))
