(ns dustin.y2022.forms.ui-req-props
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros dustin.y2022.forms.ui-req-props)))


; static props syntax in all photon fns?

; https://clojure.org/news/2021/03/18/apis-serving-people-and-programs
; (Note it is backwards for UI, in XML the kwargs come first due to deeply nested exprs)

(p/defn Dir [x]
  (let [m (datafy x)
        xs (nav m ::fs/children (::fs/children m))]
    (Explorer.
      :title (::fs/absolute-path m)
      :treelister (explorer/tree-lister xs ::fs/children #(explorer/includes-str? (::fs/name %) %2))
      :cols [::fs/name ::fs/modified ::fs/size ::fs/kind]
      :page-size 20
      :row-height 24
      ::gridsheet/grid-template-columns "auto 8em 5em 3em"
      ::dom/style {:height "calc((20 + 1) * 24px)"})))

(p/defn Explorer [& props] ; o is an entity with recursive children
  (p/client
    (let [!search (atom "") search (p/watch !search)]
      (dom/div :class "hyperfiddle-explorer-title" :text (:title props))
      (->> (ui/input ::dom/placeholder "Search files by name" ::dom/type "search"
                     :value search)
           (reset! !search))
      (dom/hr)
      (p/server
        (binding [gridsheet/Format Format]
          (GridSheet.
            ((:treelister props) search)
            ... (-> (auto-props props {})
                    (rename-keys {::row-height ::gridsheet/row-height
                                  ::page-size ::gridsheet/page-size
                                  ::columns ::gridsheet/columns})
                    (->> (merge {::gridsheet/row-height 24
                                 ::gridsheet/page-size 20
                                 ::gridsheet/columns cols})))))))))

(p/defn GridSheet [xs & props]
  (let [props (auto-props props {::dom/class "hyperfiddle-gridsheet"})
        {:keys [::columns
                ::grid-template-columns
                ::row-height ; px, same unit as scrollTop
                ::page-size #_ "tight"]} props
        client-height (* (inc (check number? page-size)) (check number? row-height))
        rows (seq xs)
        row-count (count rows)]
    (assert columns)
    (p/client
      (dom/link {:rel :stylesheet, :href "gridsheet.css"})
      (dom/div {::dom/role "grid"
                ::dom/class (::dom/class props)
                ::dom/style (merge (::dom/style props)
                                   {:height (str client-height "px")
                                    :display "grid" :overflowY "auto"
                                    :grid-template-columns (or (::grid-template-columns props)
                                                               (->> (repeat (p/server (count columns)) "1fr")
                                                                    (interpose " ") (apply str)))})}
        (let [[scroll-top scroll-height client-height'] (new (scrollview/scroll-state< dom/node))
              max-height (* row-count row-height)
              padding-bottom (js/Math.max (- max-height client-height) 0)
              clamped-scroll-top (js/Math.min scroll-top padding-bottom)
              start-row (clojure.math/ceil (/ clamped-scroll-top row-height))]
          (p/for [k columns]
            (dom/div {:role "columnheader"
                      :style {:position "sticky" #_"fixed" :top (str 0 "px")
                              :background-color "rgb(248 250 252)" :box-shadow "0 1px gray"}}
              (name k)))
          (p/server
            (when (seq rows) (check vector? (first rows)))
            (let [xs (vec (->> rows (drop start-row) (take page-size)))]
              (p/for [i (range page-size)]
                (let [[depth m] (get xs i [0 ::empty])]
                  (p/client
                    (dom/div {:role "group" :style {:display "contents"
                                                    :grid-row (inc i)}}
                      (dom/div {:role  "gridcell"
                                :style {:padding-left (-> depth (* 15) (str "px"))
                                        :position "sticky" :top (str (* row-height (inc i)) "px")
                                        :height   (str row-height "px")}}
                        (p/server (case m ::empty nil (Format. m (first columns)))))
                      (p/for [a (rest columns)]
                        (dom/div {:role  "gridcell"
                                  :style {:position "sticky" :top (str (* row-height (inc i)) "px")
                                          :height   (str row-height "px")}}
                          (p/server (case m ::empty nil (Format. m a)))))))))))
          (dom/div {:style {:padding-bottom (str padding-bottom "px")}})))
      (dom/div (pr-str {:count row-count})))))