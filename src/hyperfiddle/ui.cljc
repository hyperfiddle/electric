(ns hyperfiddle.ui
  (:require [clojure.spec.alpha :as s]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.spec :as spec]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hyperfiddle.ui :refer [spec-renderer default-renderer #_string-renderer #_keyword-renderer #_table-renderer form-renderer spec-renderer-impl form-renderer-impl default-renderer-impl render
                                                      list-renderer list-renderer-impl
                                                      table-renderer table-renderer-impl]])))

;; TODO factor inputs, find a way to pass dom props
#_(p/defn string-renderer []
  (let [value ~hf/value]
    #'(dom/input (dom/attribute "type" "text")
                 (dom/attribute "value" value)
                 (or ~(->> (dom/events dom/parent "input")
                           (m/eduction (map dom/target-value))
                           (m/reductions {} nil)
                           (m/relieve {}))
                     value))))

#_(p/defn keyword-renderer []
  (let [value ~hf/value]
    #'(dom/input (dom/attribute "type" "text")
                 (dom/attribute "value" value)
                 (dom/attribute "pattern" "^:(?:[^/\\s]+\\/)?[^/\\s]+")
                 (or ~(->> (dom/events dom/parent "input")
                           (m/eduction (map dom/target-value))
                           (m/reductions {} nil)
                           (m/relieve {}))
                     value))))

(p/def default-renderer)
(p/def default-renderer-impl #'(dom/text (pr-str ~hf/value)))

(p/def list-renderer)
(p/def list-renderer-impl
  #'(dom/ul
     (p/for [v (seq ~hf/value)]
       (dom/li ~v))))

(defn sort-by-column [cols kvs]
  (let [index (into {} (map-indexed (fn [idx v] [v idx]) cols))]
    (if (pos? (count index))
      (reduce (fn [r [k v]]
                (assoc r (get index k) [k v]))
              (vec (repeat (count index) nil))
              kvs)
      kvs)))

;; (sort-by-column [:b :a] [[:a 1] [:b 2]])

(p/def form-renderer)
(p/def form-renderer-impl []
  #'(dom/element "form"
                 (let [kvs (sort-by-column hf/columns ~hf/value)]
                   (p/for [[k v] kvs]
                     (dom/fragment
                      (dom/element "label" (dom/text (pr-str k)))
                      ~v)))))

(defn select-cols [m ks]
  (reduce (fn [r k] (conj r (get m k))) [] ks))

(p/def table-renderer)
(p/def table-renderer-impl []
  #'(dom/table
     (dom/thead
      (dom/tr
       (p/for [head hf/columns]
         (dom/th (dom/text (pr-str head))))))
     (dom/tbody
      (p/for [row (seq ~hf/value)]
        (dom/tr
         (p/for [col (sort-by-column hf/columns ~row)]
           (prn "col" col)
           (dom/td (dom/text (pr-str col)))))))))

(p/def spec-renderer)
(p/def spec-renderer-impl
  #'(let [value ~hf/value]
      (prn "value" value)
      (cond (= :hyperfiddle.q5/traversal hyperfiddle.q5/point) ~form-renderer
            (vector? value)                                    ~table-renderer
            :else                                              ~hf/sequenceM)))

(p/def render
  #'(binding [default-renderer default-renderer-impl
              form-renderer    form-renderer-impl
              list-renderer    list-renderer-impl
              table-renderer   table-renderer-impl
              spec-renderer    spec-renderer-impl]
      ~spec-renderer))

