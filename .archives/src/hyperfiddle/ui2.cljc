(ns hyperfiddle.ui2
  (:require [clojure.spec.alpha :as s]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.spec :as spec]
            [missionary.core :as m]
            [datascript.db])
  #?(:cljs (:require-macros [hyperfiddle.ui2 :refer [spec-renderer default-renderer #_string-renderer #_keyword-renderer #_table-renderer form-renderer spec-renderer-impl form-renderer-impl default-renderer-impl render
                                                      list-renderer list-renderer-impl
                                                      table-renderer table-renderer-impl
                                                      row-renderer row-renderer-impl
                                                     form-renderer-impl*
                                                     inputs-renderer
                                                     input-renderer]])))

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


(p/def list-renderer)
(p/defn list-renderer-impl []
  (dom/ul
   (p/for [v (seq ~hf/value)]
     (dom/li ~v))))

(defn sort-by-columns [cols kvs]
  (let [index (into {} (map-indexed (fn [idx v] [v idx]) cols))]
    (if (pos? (count index))
      (reduce (fn [r [k v]]
                (assoc r (get index k) [k v]))
              (vec (repeat (count index) nil))
              kvs)
      kvs)))

(def input-types {:hyperfiddle.spec.type/symbol  "text"
                  :hyperfiddle.spec.type/uuid    "text"
                  :hyperfiddle.spec.type/uri     "text"
                  :hyperfiddle.spec.type/instant "date"
                  :hyperfiddle.spec.type/boolean "checkbox"
                  :hyperfiddle.spec.type/string  "text"
                  :hyperfiddle.spec.type/bigdec  "text"
                  :hyperfiddle.spec.type/keyword "text"
                  :hyperfiddle.spec.type/ref     "text"
                  :hyperfiddle.spec.type/float   "number"
                  :hyperfiddle.spec.type/double  "number"
                  :hyperfiddle.spec.type/long    "number"})

(defn argument-type [f arg]
  (spec/type-of f arg))

(p/defn input-renderer [id type value]
  (dom/input (when id (dom/attribute "id" id))
             (dom/attribute "type" type)
             (dom/property "value" value)
             ~(->> (dom/events dom/parent "input")
                   (m/eduction (map dom/target-value))
                   (m/reductions {} nil)
                   (m/relieve {}))))

(defn property? [kw] (= "dom.property" (namespace kw)))

(p/defn input-renderer2 [props]
  (dom/input (p/for [[k v] props]
               (if (property? k)
                 (dom/property (name k) v)
                 (dom/attribute (name k) v)))
             ~(->> (dom/events dom/parent "input")
                   (m/eduction (map dom/target-value))
                   (m/reductions {} nil)
                   (m/relieve {}))))

(p/defn inputs-renderer [form inputs]
  (let [f (first form)]
    (p/for [input inputs]
      (let [param    (first input)
            renderer (second input)
            id       (str (gensym))]
        (dom/div (dom/attribute "class" "hf-input-group")
                 (dom/element "label" (dom/text param)
                              (dom/attribute "for" (str id)))
                 (binding [hf/props (assoc hf/props :html/for id
                                           :html/type (input-types (argument-type f param)))]
                   (if (some? renderer)
                     ~renderer
                     (p/$ input-renderer id (input-types (argument-type f param)) ""))))))))

(defn schema-attr [db ?a]
  (condp = (type db)
    datascript.db.DB (get (:schema db) ?a)))

(p/def default-renderer)
(p/defn default-renderer-impl []
  (if-let [attr hf/attribute]
    (let [type (:hf/valueType (schema-attr hf/*$* attr))]
      (p/$ input-renderer nil (input-types (spec/valueType->type type)) ~hf/value))
    (dom/text (pr-str ~hf/value))))

(p/def form-renderer)
(p/defn form-renderer-impl []
  (dom/element "form"
               (let [inputs (:hyperfiddle.q5/inputs (meta ~hf/value))
                     kvs    (sort-by-columns hf/columns ~hf/value)]
                 (prn "inputs" inputs)
                 (p/for [[k v] kvs]
                   (let [params (get inputs k)]
                     (dom/element "fieldset"
                                  (dom/element "legend" (dom/text (first k)))
                                  (binding [hf/args (zipmap (map first params)
                                                            (p/$ inputs-renderer k params))]
                                    ~v)))))))

(p/def form-renderer-impl* form-renderer-impl)

(p/def row-renderer)
(p/defn row-renderer-impl []
  (dom/tr
   (do
     (prn "VALUE" ~hf/value)
     (let [kvs (sort-by-columns hf/columns ~hf/value)]
       (binding [form-renderer-impl form-renderer-impl*]
         (p/for [[k v] kvs]
           (binding [hf/attribute k]
             (dom/td (if v ~v (dom/text "not found"))))))))))

(p/def table-renderer)
(p/defn table-renderer-impl []
  (do
    (js/console.log "TABLE")
    (dom/table
     (dom/thead
      (dom/tr
       (p/for [col hf/columns]
         (dom/th (dom/text (pr-str col))))))
     (dom/tbody
      (p/for [row (seq ~hf/value)]
        (binding [form-renderer-impl row-renderer-impl]
          ~row))))))

(p/def spec-renderer)
(p/defn spec-renderer-impl []
  (cond (map? ~hf/value)    ~form-renderer
        (vector? ~hf/value) ~table-renderer
        :else               ~default-renderer))

(p/def render
  #'(binding [default-renderer default-renderer-impl
              form-renderer    form-renderer-impl
              list-renderer    list-renderer-impl
              table-renderer   table-renderer-impl
              spec-renderer    spec-renderer-impl]
      ~spec-renderer))

