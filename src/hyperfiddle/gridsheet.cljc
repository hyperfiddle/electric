(ns hyperfiddle.gridsheet
  (:require clojure.math
            [contrib.data :refer [unqualify auto-props round-floor]]
            [clojure.spec.alpha :as s]
            [hyperfiddle.logger :as log]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            #?(:cljs [hyperfiddle.scrollview :as scrollview :refer [scroll-state<]])
            [hyperfiddle.rcf :refer [tests]]
            #?(:cljs goog.object))
  #?(:cljs (:require-macros hyperfiddle.gridsheet)))

(p/def Format (p/server (p/fn [m a v] (pr-str v))))

(p/defn GridSheet [xs props]
  (let [{:keys [::columns
                ::grid-template-columns
                ::row-height ; px, same unit as scrollTop
                ::page-size]} ; tight
        (auto-props (namespace ::x) props {})
        rows (seq xs)
        row-count (count rows)]
    (assert row-height)
    (assert page-size)
    (assert columns)
    (p/client
      (dom/div {:role "grid"
                :class (::dom/class props)
                :style {:display "grid" :overflowY "auto"
                        :grid-template-columns (or (::grid-template-columns props)
                                                   (->> (repeat (p/server (count columns)) "1fr")
                                                        (interpose " ") (apply str)))}}
        (let [[scrollTop scrollHeight clientHeight] (new (scrollview/scroll-state< dom/node))
              max-height (* row-count row-height)

              ; don't scroll past the end
              clamped-scroll-top (js/Math.min scrollTop (- max-height clientHeight))

              start-row (clojure.math/ceil (/ clamped-scroll-top row-height))

              ; batch pagination to improve latency
              ; (does reducing network even help or just making loads happen offscreen?)
              ; clamp start to the nearest page
              start-row-page-aligned (round-floor start-row page-size)]
          #_(println [:scrollTop scrollTop :scrollHeight scrollHeight :clientHeight clientHeight
                    :start-row start-row :start-row-page-aligned start-row-page-aligned
                    :take page-size :max-height max-height])

          (p/for [k columns]
            (dom/div {:role "columnheader"
                      :style {:position "sticky" #_"fixed" :top (str 0 "px")
                              :background-color "rgb(248 250 252)" :box-shadow "0 1px gray"}}
              (name k)))

          (let [!!rows (vec (repeatedly page-size (partial atom nil)))]
            (p/for [i (range page-size)]
              (dom/div {:role "group" :style {:display "contents"}}
                (let [[a & as] columns
                      [depth ?Render] (p/watch (get !!rows i))]
                  (dom/div {:role "gridcell"
                            :style {:padding-left (-> depth (* 15) (str "px"))
                                    :position "sticky" :top (str (* row-height (inc i)) "px")
                                    :height (str row-height "px")}}
                    (some-> ?Render (new a)))
                  (p/for [[j a] (map-indexed vector as)]
                    (dom/div {:role "gridcell"
                              :style {:position "sticky" :top (str (* row-height (inc i)) "px")
                                      :height (str row-height "px")}}
                      (some-> ?Render (new a)))))))

            (dom/div {:style {:padding-bottom (str (- max-height clientHeight) "px")}}) ; scrollbar

            (p/server
              (let [xs (->> rows (drop start-row) (take page-size))]
                (p/for-by first [[i [depth m]] (map vector (range) xs)]
                  (let [[a & as] columns]
                    (p/client
                      (reset! (get-in !!rows [i])
                              [depth (p/fn [a] (p/server (Format. m a (a m))))])))))))))
      (dom/div (pr-str {:count row-count})))))

(p/defn ^:deprecated TableSheet
  "Perhaps useful to keep around? Prefer the css-grid sheet
  This does not align column headers with body columns due to position:sticky layout"
  [xs props]
  (let [{:keys [::columns
                ::row-height ; px, same unit as scrollTop
                ::page-size]} ; tight
        (auto-props (namespace ::x) props {})
        rows (seq xs)
        row-count (count rows)]
    (p/client
      (dom/table {:style {:display "block" :overflowY "auto"
                          :height "500px"}} ; fixme
        (let [[scrollTop scrollHeight clientHeight] (new (scrollview/scroll-state< dom/node))
              max-height (* row-count row-height)

              ; don't scroll past the end
              clamped-scroll-top (js/Math.min scrollTop (- max-height clientHeight))

              start-row (clojure.math/ceil (/ clamped-scroll-top row-height))

              ; batch pagination to improve latency
              ; (does reducing network even help or just making loads happen offscreen?)
              ; clamp start to the nearest page
              start-row-page-aligned (round-floor start-row page-size)

              ;padding-top (* start-row-page-aligned row-height)
              ;padding-bottom (- max-height padding-top clientHeight)
              padding-top #_clamped-scroll-top (* start-row row-height)
              padding-bottom (- max-height padding-top clientHeight)]
          #_(println [:scrollTop scrollTop :scrollHeight scrollHeight :clientHeight clientHeight
                      :start-record start-row-page-aligned :start-row start-row :take page-size
                      ; padding  not needed if max-height is set
                      :max-height max-height
                      :padding-top padding-top :padding-bottom padding-bottom])

          (dom/thead {:style {:position "sticky" #_"fixed" :top "0"
                              ; :position breaks column layout - detaches thead from tbody layout
                              :background-color "rgb(248 250 252)" :box-shadow "0 1px gray"}}
            (p/for [k columns] (dom/td (name k))))

          (dom/tbody {:style {:height (str max-height "px")}}
            (let [!!rows (vec (repeatedly page-size (partial atom nil)))]
              #_(dom/div {:style {:padding-top (str padding-top "px")}})
              (p/for [i (range page-size)]
                (dom/tr {:style {:position "fixed"
                                 :margin-top (str (* row-height i) "px")
                                 :height (str row-height "px")}} ; freeze layout
                  (let [[a & as] columns
                        [depth ?Render] (p/watch (get !!rows i))]
                    (dom/td {:style {:padding-left (-> depth (* 15) (str "px"))}}
                      (some-> ?Render (new a)))
                    (p/for [a as]
                      (dom/td (some-> ?Render (new a)))))))
              #_(dom/div {:style {:padding-bottom (str padding-bottom "px")}})

              (p/server
                (let [xs (->> rows (drop start-row) (take page-size))]
                  (p/for-by first [[i [depth m]] (map vector (range) xs)]
                    (p/client
                      (reset! (get-in !!rows [i])
                              [depth (p/fn [a] (p/server (Format. m a (a m))))])))))))))
      (dom/div (pr-str {:count row-count})))))

; How to do transactionally with a fragment to avoid the churn? (for variable infinite seq)

(p/defn RenderTableInfinite "working" [xs props]
  (let [{:keys [::columns
                ::row-height ; px, same unit as scrollTop
                ::page-size]} ; you want this loose, like 100
        (auto-props (namespace ::x) props {::page-size 100})
        !pages (atom 1) pages (p/watch !pages)]
    (p/client
      (dom/table {:style {:display "block" :overflow "hidden auto"
                          :height "500px"}}
        (let [[scrollTop scrollHeight clientHeight] (new (scrollview/scroll-state< dom/node))]
          (when (> scrollTop (- scrollHeight clientHeight clientHeight)) ; scrollLoadThreshold = clientHeight
            (p/server (swap! !pages inc))))
        (dom/thead {:style {:position "sticky" :top "0" :background-color "rgb(248 250 252)"
                            :box-shadow "0 1px gray"}}
          (p/for [k columns] (dom/td (name k))))
        (dom/tbody
          (p/server
            (if-let [rows (take (* pages page-size) (seq xs))]
              (p/for [[depth m] rows]
                (let [[a & as] columns]
                  (p/client
                    (dom/tr
                      (dom/td {:style {:padding-left (-> depth (* 15) (str "px"))}}
                        (p/server (Format. m a (a m))))
                      (p/server
                        (p/for [a as]
                          (p/client (dom/td (p/server (Format. m a (a m)))))))))))
              (p/client (dom/div {:class "no-results"} "no results matched")))))))))