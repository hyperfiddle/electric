(ns hyperfiddle.electric-scroll0
  (:require [clojure.math :as math]
            [contrib.assert :refer [check]]
            [contrib.data :refer [clamp window]]
            [contrib.missionary-contrib :as mx]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.rcf :as rcf]
            [hyperfiddle.incseq :as i]
            [missionary.core :as m]
            [hyperfiddle.electric-local-def3 :as l]))

#?(:cljs (defn scroll-state [scrollable]
           (->> (m/observe
                  (fn [!]
                    (! [0 0 0])
                    (let [sample (fn [] (! [(.. scrollable -scrollTop) ; optimization - detect changes (pointless)
                                            (.. scrollable -scrollHeight) ; snapshot height to detect layout shifts in flipped mode
                                            (.. scrollable -clientHeight)]))] ; measured viewport height (scrollbar length)
                      (.addEventListener scrollable "scroll" sample #js {"passive" true})
                      #(.removeEventListener scrollable "scroll" sample))))
             (mx/throttle 16) ; RAF interval
             (m/relieve {}))))

#?(:cljs (defn resize-observer [node]
           (m/relieve {}
             (m/observe (fn [!] (! [(.-clientHeight node)
                                    (.-clientWidth node)])
                          (let [obs (new js/ResizeObserver
                                      (fn [entries]
                                        (let [content-box-size (-> entries (aget 0) .-contentBoxSize (aget 0))]
                                          (! [(.-blockSize content-box-size)
                                              (.-inlineSize content-box-size)]))))]
                            (.observe obs node) #(.unobserve obs)))))))

#?(:cljs (defn compute-overquery [overquery-factor record-count offset limit]
           (let [q-limit (* limit overquery-factor)
                 occluded (clamp (- q-limit limit) 0 record-count)
                 q-offset (clamp (- offset (math/floor (/ occluded overquery-factor))) 0 record-count)]
             [q-offset q-limit])))

#?(:cljs (defn compute-scroll-window [row-height record-count clientHeight scrollTop overquery-factor]
           (let [padding-top 0 ; e.g. sticky header row
                 limit (math/ceil (/ (- clientHeight padding-top) row-height)) ; aka page-size
                 offset (int (/ (clamp scrollTop 0 (* record-count row-height)) ; prevent overscroll past the end
                               row-height))]
             (compute-overquery overquery-factor record-count offset limit))))

#?(:cljs (defn scroll-window ; returns [offset, limit]
           [row-height record-count node
            & {:keys [overquery-factor]
               :or {overquery-factor 1}}]
           (m/cp
             (let [[clientHeight] (m/?< (resize-observer node))
                   [scrollTop] (m/?< (scroll-state node))] ; smooth scroll has already happened, cannot quantize
               (compute-scroll-window row-height record-count clientHeight scrollTop overquery-factor)))))

(e/defn Scroll-window [row-height record-count node #_& {:as props}]
  (e/client (doto (e/input (scroll-window row-height record-count node props))
              #_(prn 'Scroll-window))))

(e/defn Spool2 [cnt xs! offset limit]
  (->> xs!
    (map vector (cycle (range limit)))
    (window cnt offset limit)
    (e/diff-by first)))

(e/defn Spool [cnt xs! offset limit]
  (->> (map-indexed vector xs!)
    (window cnt offset limit)
    (e/diff-by #(mod (first %) limit))))

(e/defn Unrotate [limit xs]
  (let [s (i/spine)]
    (e/for [[i x] xs]
      (s (mod i limit) {} [i x])
      #_(e/amb)) ; undo permutations
    (e/join s)))

#_
(e/defn Spool-scroll [record-count xs row-height node]
  (Spool record-count xs (Scroll-window row-height record-count node)))

(e/defn Scroll-indexed-headless
  "random access (fixed height, counted, indexed)"
  [viewport-node xs!
   #_& {:keys [record-count row-height overquery-factor]
        :or {overquery-factor 1}}]
  (let [record-count (or record-count (count xs!))
        row-height (check row-height) ; todo measure, account for browser zoom level
        [offset limit] (Scroll-window row-height record-count viewport-node {:overquery-factor overquery-factor})]
    {::Spool (e/fn [] (Spool record-count xs! offset limit)) ; site neutral, caller chooses
     ::Offset (e/fn [] ; isolate animating value to not rebuild hashmap - micro optimization
                (identity offset)) ; experimental: allow user to artificially delay offset if needed for UX
     ::limit limit ::record-count record-count ::row-height row-height}))

(e/defn TableScrollFixedCounted
  [xs! TableBody
   #_& {:keys [row-height]
        :as props}])

(letfn [(index-ring [size offset]
          (let [start (- size offset)]
            (mapv #(+ offset (mod % size)) (range start (+ size start)))))]
  (e/defn IndexRing [size offset]
    (e/diff-by {} (index-ring size offset))))

(rcf/tests
  (let [!offset (atom 0)]
    (rcf/with ((l/single {} (e/Tap-diffs rcf/tap (IndexRing 7 (e/watch !offset)))) {} {})
      rcf/% := {:degree 7, :permutation {}, :grow 7, :shrink 0, :change {0 0, 1 1, 2 2, 3 3, 4 4, 5 5, 6 6}, :freeze #{}}
      (swap! !offset inc)
      rcf/% := {:degree 7, :permutation {}, :grow 0, :shrink 0, :change {0 7}, :freeze #{}}
      (swap! !offset + 2)
      rcf/% := {:degree 7, :permutation {}, :grow 0, :shrink 0, :change {1 8, 2 9}, :freeze #{}}
      )))
