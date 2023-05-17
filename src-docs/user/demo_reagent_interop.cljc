
;; This demo requires `npm install`

(ns user.demo-reagent-interop
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            #?(:cljs [reagent.core :as r])
            #?(:cljs ["recharts" :refer [ScatterChart, Scatter, LineChart, Line, XAxis, YAxis, CartesianGrid]])
            #?(:cljs ["react-dom/client" :as ReactDom])))

(def ReactRootWrapper
  #?(:cljs
     (r/create-class
       {:component-did-mount (fn [this] (js/console.log "mounted"))
        :render (fn [this]
                  (let [[_ Component & args] (r/argv this)]
                    (into [Component] args)))})))

(def react-root-hook "See `e/with`"
  #?(:clj  dom/unsupported
     :cljs (fn ([x] (.unmount x))
             ([x y] (.insertBefore (.-parentNode x) x y)))))

(defn create-root
  "See `https://reactjs.org/docs/react-dom-client.html#createroot`"
  ([node] (create-root node (str (gensym))))
  ([node identifier-prefix] #?(:cljs (ReactDom/createRoot node #js {:identifierPrefix identifier-prefix}))))

(defn render [root & args] #?(:cljs (.render root (r/as-element (into [ReactRootWrapper] args)))))

(defmacro with-reagent [& args]
  `(dom/div  ; React will hijack this element and empty it.
     (binding [dom/node (create-root dom/node)]
       (new (e/hook react-root-hook dom/node
              (e/fn [] dom/keepalive
                (render dom/node ~@args)))))))

;;;;;;;;;;;;;;;;;;;
;; Reagent World ;;
;;;;;;;;;;;;;;;;;;;

(defn TinyLineChart [data]
  #?(:cljs
     [:> LineChart {:width 400 :height 200 :data (clj->js data)}
      [:> CartesianGrid {:strokeDasharray "3 3"}]
      [:> XAxis {:dataKey "name"}]
      [:> YAxis]
      [:> Line {:type "monotone", :dataKey "pv", :stroke "#8884d8", :strokeWidth 2}]
      [:> Line {:type "monotone", :dataKey "uv", :stroke "#82ca9d", :strokeWidth 2}]]))

(defn MousePosition [x y]
  #?(:cljs
     [:div
      [:h2 "Mouse coordinates"]
      [:> ScatterChart {:width 300 :height 300 :margin #js{:top 20, :right 20, :bottom 20, :left 20}}
       [:> CartesianGrid {:strokeDasharray "3 3"}]
       [:> XAxis {:type "number", :dataKey "x", :unit "px", :domain #js[0 2000]}]
       [:> YAxis {:type "number", :dataKey "y", :unit "px", :domain #js[0 2000]}]
       [:> Scatter {:name "Mouse position", :data (clj->js [{:x x, :y y}]), :fill "#8884d8"}]]]))

;;;;;;;;;;;;;;;;;;;;;;
;; Electric Clojure ;;
;;;;;;;;;;;;;;;;;;;;;;

(e/defn ReagentInterop []
  (e/client
    (dom/h1 (dom/text "Reagent/React Interop")) 
    (let [[x y] [(.-clientX e/dom-mousemove)
                 (.-clientY e/dom-mousemove)]]
      ;; Adapted from https://recharts.org/en-US/examples/TinyLineChart
      (with-reagent TinyLineChart [{:name "Page A", :uv 4000, :pv 2400, :amt 2400}
                                   {:name "Page B", :uv 3000, :pv 1398, :amt 2210}
                                   {:name "Page C", :uv 2000, :pv (* 2 y), :amt 2290}  ; inject value
                                   {:name "Page D", :uv 2780, :pv 3908, :amt 2000}
                                   {:name "Page E", :uv 1890, :pv 4800, :amt 2181}
                                   {:name "Page F", :uv 2390, :pv 3800, :amt 2500}
                                   {:name "Page G", :uv 3490, :pv 4300, :amt 2100}])
      (with-reagent MousePosition x y))))
