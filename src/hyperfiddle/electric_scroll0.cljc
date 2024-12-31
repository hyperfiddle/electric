(ns hyperfiddle.electric-scroll0
  (:require [clojure.math :as math]
            [contrib.data :refer [clamp window]]
            [contrib.missionary-contrib :as mx]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.rcf :as rcf]
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
                            (.observe obs node) #(.unobserve obs node)))))))

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

(e/defn Spool2 [cnt xs! offset limit] ; legacy
  (->> xs!
    (map vector (cycle (range limit)))
    (window cnt offset limit)
    (e/diff-by first)))

(e/defn Spool [cnt xs! offset limit] ; legacy
  (->> (map-indexed vector xs!)
    (window cnt offset limit)
    (e/diff-by #(mod (first %) limit))))

(defn index-ring
  "Return a vector of numbers of size `size` containing numbers [0..size[ shifted by `offset`, in the direction of sgn(offset).
  Contrary to usual \"sliding window\" shifting, where all slots shift one place left or right, this ring shifts as a tape. 
  Values shift, not slots. Allowing for stable indexing of a sliding window.
  e.g.: Usual : [1 2 3] -> [2 3 4] ; shift one right - all values changed, as if all slots shifted.
        Tape:   [1 2 3] -> [4 2 3] ; 4 replaced 1 at slot 0. Slots 1 and 2 untouched.
  Example: assuming size = 5, and offset in [0,1,2,3,4,5]
  Offset
  0 -> [0 1 2 3 4] ; 5 slots, initial state
  1 -> [5 1 2 3 4] ; shift one right – 5 replaces 0 at slot 0, 1 2 3 4 untouched
  2 -> [5 6 2 3 4] ; shift one right – 6 replaces 1 at slot 1, 5 and the rest untouched
  3 -> [5 6 7 3 4]
  4 -> [5 6 7 8 4]
  5 -> [5 6 7 8 9] ; offset = size, full window slide, all values shifted, but no slot shifted.
  "
  [size offset]
  (let [start (- size offset)]
    (mapv #(+ offset (mod % size)) (range start (+ size start)))))

(rcf/tests
  (let [size 7]
    (mapv #(index-ring size %) (range (inc size)))
    := [[0  1  2  3  4  5  6]
        [7  1  2  3  4  5  6]
        [7  8  2  3  4  5  6]
        [7  8  9  3  4  5  6]
        [7  8  9  10 4  5  6]
        [7  8  9  10 11 5  6]
        [7  8  9  10 11 12 6]
        [7  8  9  10 11 12 13]]

    (range 0 (- (inc size)) -1) := '(0 -1 -2 -3 -4 -5 -6 -7) ; for reference
    (mapv #(index-ring size %) (range 0 (- (inc size)) -1))
    := [[ 0  1  2  3  4  5  6]
        [ 0  1  2  3  4  5 -1]
        [ 0  1  2  3  4 -2 -1]
        [ 0  1  2  3 -3 -2 -1]
        [ 0  1  2 -4 -3 -2 -1]
        [ 0  1 -5 -4 -3 -2 -1]
        [ 0 -6 -5 -4 -3 -2 -1]
        [-7 -6 -5 -4 -3 -2 -1]]))

(let [index-ring index-ring] ; FIXME without this let, below rcf test crashes at the repl
  (e/defn IndexRing [size offset] (e/diff-by {} (index-ring size offset))))

(rcf/tests
  (let [size 7
        !offset (atom 0)]
    (rcf/with ((l/single {} (e/Tap-diffs rcf/tap (IndexRing size (e/watch !offset)))) {} {})
      rcf/% := {:degree 7, :permutation {}, :grow 7, :shrink 0, :change {0 0, 1 1, 2 2, 3 3, 4 4, 5 5, 6 6}, :freeze #{}}
      (swap! !offset inc)
      rcf/% := {:degree 7, :permutation {}, :grow 0, :shrink 0, :change {0 7}, :freeze #{}}
      (swap! !offset + 2)
      rcf/% := {:degree 7, :permutation {}, :grow 0, :shrink 0, :change {1 8, 2 9}, :freeze #{}}
      (reset! !offset size) ; check offset = size
      rcf/% := {:degree 7, :permutation {}, :grow 0, :shrink 0, :change {3 10, 4 11, 5 12, 6 13}, :freeze #{}}
      (swap! !offset inc) ; check offset > size
      rcf/% := {:degree 7, :permutation {}, :grow 0, :shrink 0, :change {0 14}, :freeze #{}}
      ;; negative offset, no use case for now but check for correctness
      (reset! !offset 0) ; reset to initial state
      rcf/% := {:degree 7, :permutation {}, :grow 0, :shrink 0, :change {0 0, 1 1, 2 2, 3 3, 4 4, 5 5, 6 6}, :freeze #{}}
      (swap! !offset dec) ; check offset < 0
      rcf/% := {:degree 7, :permutation {}, :grow 0, :shrink 0, :change {6 -1}, :freeze #{}}
      (reset! !offset -7)
      rcf/% := {:degree 7, :permutation {}, :grow 0, :shrink 0, :change {0 -7, 1 -6, 2 -5, 3 -4, 4 -3, 5 -2}, :freeze #{}}
      )))

