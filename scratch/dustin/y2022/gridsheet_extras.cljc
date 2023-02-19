(ns dustin.y2022.gridsheet-extras
  "todo deprecate, use HFQL grid"
  #?(:cljs (:require-macros dustin.y2022.gridsheet-extras))
  (:require clojure.math
            [contrib.assert :refer [check]]
            [contrib.data :refer [auto-props round-floor]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.history :as router] ; todo remove
            #?(:cljs goog.object)))

(e/defn GridSheet [xs props]
  (let [props (auto-props props
                {::row-height 24
                 ::page-size 20})
        _ (def props props)
        {:keys [::Format
                ::columns
                ::grid-template-columns
                ::row-height ; px, same unit as scrollTop
                ::page-size #_ "tight"]} props
        Format (or Format (e/fn [m a] (e/client (dom/text (pr-str (a m))))))
        client-height (* (inc (check number? page-size)) (check number? row-height))
        rows (seq xs)
        row-count (count rows)]
    (assert columns "gridsheet: ::columns prop is required")
    (e/client
      (dom/div (dom/props {:role "grid"
                           :class (e/server (::dom/class props))
                           :style (merge (e/server (::dom/style props))
                                    {:height (str client-height "px")
                                     :display "grid" :overflowY "auto"
                                     :grid-template-columns (or (e/server (::grid-template-columns props))
                                                              (->> (repeat (e/server (count columns)) "1fr")
                                                                (interpose " ") (apply str)))})})
        (let [[scroll-top scroll-height client-height'] (new (ui/scroll-state< dom/node))
              max-height (* row-count row-height)
              padding-bottom (js/Math.max (- max-height client-height) 0)

              ; don't scroll past the end
              clamped-scroll-top (js/Math.min scroll-top padding-bottom)

              start-row (clojure.math/ceil (/ clamped-scroll-top row-height))

              ; batch pagination to improve latency
              ; (does reducing network even help or just making loads happen offscreen?)
              ; clamp start to the nearest page
              start-row-page-aligned (round-floor start-row page-size)]
          #_(println [:scrollTop scroll-top :scrollHeight scroll-height :clientHeight client-height
                      :padding-bottom padding-bottom
                      :start-row start-row :start-row-page-aligned start-row-page-aligned
                      :take page-size :max-height max-height])

          (e/for [k columns]
            (dom/div (dom/props {:role "columnheader"
                                 :style {:position "sticky" #_"fixed" :top (str 0 "px")
                                         :background-color "rgb(248 250 252)" :box-shadow "0 1px gray"}})
              (dom/text (name k))))

          ; userland could format the row, no need
          ; for grid to be aware of columns, it's just vertical scroll.
          ; horizontal scroll changes things.
          ; except for the tricky styles ...
          (e/server
            (when (seq rows) (check vector? (first rows)))
            (let [xs (vec (->> rows (drop start-row) (take page-size)))]
              (e/for [i (range page-size)]
                (let [[depth m] (get xs i [0 ::empty])]
                  (e/client
                    (dom/div (dom/props {:role "group" :style {:display "contents"
                                                               :grid-row (inc i)}})
                      (dom/div (dom/props {:role "gridcell"
                                           :style {:padding-left (-> depth (* 15) (str "px"))
                                                   :position "sticky" :top (str (* row-height (inc i)) "px")
                                                   :height (str row-height "px")}})
                        (e/server (case m ::empty nil (Format. m (first columns))))) ; for effect
                      (e/for [a (rest columns)]
                        (dom/div (dom/props {:role "gridcell"
                                             :style {:position "sticky" :top (str (* row-height (inc i)) "px")
                                                     :height (str row-height "px")}})
                          (e/server (case m ::empty nil (Format. m a))))))))))) ; for effect
          (dom/div (dom/props {:style {:padding-bottom (str padding-bottom "px")}})))) ; scrollbar
      (dom/div (dom/text (pr-str {:count row-count}))))))

(e/defn Explorer [query-fn props] ; o is an entity with recursive children
  (e/client
    (let [{:keys [::search] :as s} router/route]
      (ui/input search (e/fn V! [v] (router/swap-route! assoc ::search v)) ; todo (swap! router/!route assoc ::search v)
        (dom/props {:placeholder "Search" :type "search"}))
      (dom/hr)
      (e/server
        (GridSheet. (query-fn search) props)))))

(e/defn ^:deprecated TableSheet
  "Perhaps useful to keep around? Prefer the css-grid sheet
  This does not align column headers with body columns due to position:sticky layout"
  [xs props]
  (let [{:keys [::columns
                ::row-height ; px, same unit as scrollTop
                ::page-size]} ; tight
        (auto-props props)
        rows (seq xs)
        row-count (count rows)]
    (e/client
      (dom/table (dom/props {:style {:display "block" :overflowY "auto"
                                     :height "500px"}}) ; fixme
        (let [[scrollTop scrollHeight clientHeight] (new (ui/scroll-state< dom/node))
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

          (dom/thead
            (dom/props {:style {:position "sticky" #_"fixed" :top "0"
                                ; :position breaks column layout - detaches thead from tbody layout
                                :background-color "rgb(248 250 252)" :box-shadow "0 1px gray"}})
            (e/for [k columns]
              (dom/td (dom/text (name k)))))

          (dom/tbody
            (dom/props {:style {:height (str max-height "px")}})
            (let [!!rows (vec (repeatedly page-size (partial atom nil)))]
              #_(dom/div (dom/props {:style {:padding-top (str padding-top "px")}}))
              (e/for [i (range page-size)]
                (dom/tr
                  (dom/props {:style {:position "fixed"
                                      :margin-top (str (* row-height i) "px")
                                      :height (str row-height "px")}}) ; freeze layout
                  (let [[a & as] columns
                        [depth ?Render] (e/watch (get !!rows i))]
                    (dom/td
                      (dom/props {:style {:padding-left (-> depth (* 15) (str "px"))}})
                      (some-> ?Render (new a))) ; for effect
                    (e/for [a as]
                      (dom/td (some-> ?Render (new a))))))) ; for effect
              #_(dom/div (dom/props {:style {:padding-bottom (str padding-bottom "px")}}))

              (e/server
                (let [xs (->> rows (drop start-row) (take page-size))]
                  (e/for-by first [[i [depth m]] (map vector (range) xs)]
                    (e/client
                      (reset! (get-in !!rows [i])
                        [depth (e/fn [a] (e/server (Format. m a)))])))))))))
      (dom/div (pr-str {:count row-count})))))

; How to do transactionally with a fragment to avoid the churn? (for variable infinite seq)

(e/defn RenderTableInfinite [xs props]
  (let [{:keys [::columns
                ::row-height ; px, same unit as scrollTop
                ::page-size]} ; you want this loose, like 100
        (auto-props props {::page-size 100})
        !pages (atom 1) pages (e/watch !pages)]
    (e/client
      (dom/table
        (dom/props {:style {:display "block" :overflow "hidden auto"
                            :height "500px"}})
        (let [[scrollTop scrollHeight clientHeight] (new (ui/scroll-state< dom/node))]
          (when (> scrollTop (- scrollHeight clientHeight clientHeight)) ; scrollLoadThreshold = clientHeight
            (e/server (swap! !pages inc))))
        (dom/thead
          (dom/props {:style {:position "sticky" :top "0" :background-color "rgb(248 250 252)"
                              :box-shadow "0 1px gray"}})
          (e/for [k columns]
            (dom/td (dom/text (name k)))))
        (dom/tbody
          (e/server
            (if-let [rows (take (* pages page-size) (seq xs))]
              (e/for [[depth m] rows]
                (let [[a & as] columns]
                  (e/client
                    (dom/tr
                      (dom/td
                        (dom/props {:style {:padding-left (-> depth (* 15) (str "px"))}})
                        (e/server (Format. m a))) ; for effect
                      (e/server
                        (e/for [a as]
                          (e/client (dom/td (e/server (Format. m a)))))))))) ; for effect
              (e/client
                (dom/div
                  (dom/props {:class "no-results"})
                  (dom/text "no results matched"))))))))))
