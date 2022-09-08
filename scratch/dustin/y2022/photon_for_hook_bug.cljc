(ns dustin.y2022.photon-for-hook-bug
  (:require [hyperfiddle.scrollview :as scrollview]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :refer [tests ! % with]])
  #?(:cljs (:require-macros dustin.y2022.photon-for-hook-bug)))


(p/defn Bug []
  (p/server
    (let [rows (range 100)
          row-count (count rows)
          row-height 24 ; px, same unit as scrollTop
          page-size 20] ; tight
      (p/client
        (dom/div {:style {:display "grid"
                          :grid-template-columns (apply str (interpose " " (repeat 4 "auto")))
                          :overflowY "auto" :height "200px"}}

          ; need to slow the start parameter down so that it cannot resonate with the scrollTop

          (let [[scrollTop scrollHeight clientHeight] (new (scrollview/scroll-state< dom/node))
                max-height (* row-count row-height)
                clamped-scroll-top (js/Math.min scrollTop (- max-height clientHeight))
                start (js/Math.floor (/ clamped-scroll-top row-height))]
            (println [:scrollTop scrollTop :scrollHeight scrollHeight :clientHeight clientHeight :max-height max-height :clamped-scroll-top clamped-scroll-top :start start])
            (p/server
              (if-let [rows (->> rows (drop start) (take page-size))]
                (p/for [x rows]
                  (p/client
                    (dom/div "1")
                    (dom/div "2")
                    (dom/div "3")
                    (dom/div "4")))
                (p/client (dom/div {:class "no-results"} "no results matched"))))))
        (dom/div (pr-str {:count row-count}))))))
