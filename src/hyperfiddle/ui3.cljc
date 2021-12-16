(ns hyperfiddle.ui3
  (:refer-clojure :exclude [boolean])
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.spec :as spec]
            [missionary.core :as m]
            [datascript.db]
            #?(:clj [hyperfiddle.q6 :as hfql])
            [hyperfiddle.dev.logger :as log]
            [hfdl.impl.runtime] ;; FIXME remove
            )
  #?(:cljs (:require-macros [hyperfiddle.q6 :as hfql]
                            [hyperfiddle.ui3 :refer [render
                                                       spec-renderer spec-renderer-impl
                                                       default-renderer default-renderer-impl
                                                       string-renderer
                                                       form form-impl form-impl*
                                                       table table-impl
                                                       row row-impl
                                                       table-picker table-picker-impl -table-picker-props
                                                       options-picker options-picker-impl
                                                       row-picker row-picker-impl
                                                       input
                                                       boolean boolean-impl
                                                       render-inputs
                                                       render-options
                                                       typeahead typeahead-impl
                                                       link
                                                       with-spec-render
                                                       render-mode-selector]])))

;;;;;;;;;;;;
;; COLORS ;;
;;;;;;;;;;;;

(defn hsl [h s l] (str "hsl(" h "," s "%," l "%)"))

(defn color
  "Hash a value into an harmonious color.
  See `http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/`,
  `https://webcache.googleusercontent.com/search?q=cache:qmCbllpQTP8J:https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/+&cd=1&hl=fr&ct=clnk&gl=fr`"
  [x]
  (let [golden-ratio 0.618033988749895
        seed         0.3100632204946232 ; DG: i liked these colors, GG: I don’t like the one for "$" (default)
        ]
    (if (nil? x)
      "#ccc"
      (hsl (* 360 (mod (+ seed (* (hash x) golden-ratio)) 1))
           55  #_"Too bright hurts the eyes"
           70) #_"Medium gray (50) can be read on white and black backgrounds")))

;;;;;;;;;;;;;;;;;
;; UI ELEMENTS ;;
;;;;;;;;;;;;;;;;;

(p/def table)
(p/def row)
(p/def form)
(p/def table-picker)
(p/def row-picker)
(p/def options-picker)
(p/def boolean)

(p/defn link []
  (dom/element "a"
               (dom/attribute "href" (pr-str ~hf/value))
               (dom/text (pr-str ~hf/value))))

;; DEPRECATED
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

(defn argument-type [f arg] (spec/type-of f arg))

;; (defn dom-prop? [kw] (str/starts-with? (namespace kw) "dom."))

(defn property? [kw] (= "dom.property" (namespace kw)))

(p/defn input [props extractor]
  (dom/input (p/for [[k v] props]
               (cond
                 (= :dom.property/style k) (dom/style v)
                 (property? k)             (dom/property (name k) v)
                 :else                     (dom/attribute (name k) v)))
             ~(->> (dom/events dom/parent "input")
                   (m/eduction (map extractor))
                   (m/reductions {} nil)
                   (m/relieve {}))))

(p/defn boolean-impl []
  (dom/div (dom/text ~hf/value))
  (p/$ input {:dom.attribute/type "checkbox"
              :dom.property/checked ~hf/value}
       dom/target-checked))

(defn set-state! [!atom v] (reset! !atom v))

(p/defn render-mode-selector []
  (let [state (atom ::hfql/default)]
    (dom/div (dom/class "hf-render-mode-selector")
             (dom/button (dom/text "default")
                         ~(->> (dom/events dom/parent "click")
                               (m/eduction (map (constantly ::hfql/default))
                                           (map (partial set-state! state)))
                               (m/reductions {} nil)
                               (m/relieve {})))
             (dom/button (dom/text "user")
                         ~(->> (dom/events dom/parent "click")
                               (m/eduction (map (constantly ::hfql/user))
                                           (map (partial set-state! state)))
                               (m/reductions {} nil)
                               (m/relieve {}))))
    ~(m/watch state)))

(p/def typeahead)
(p/defn typeahead-impl []
  ;; (binding [hf/render hf/sequenceM])
  (let [id      (str (gensym))
        options (::hf/options hf/props)
        label   (::hf/option-label hf/props)]
    (do (log/info "TYPEAHEAD" {:id id, :options options, :label label})
        (dom/fragment
         (let [value  {} #_~hf/value
               value' (p/$ input {:dom.attribute/type    (:dom.attribute/type hf/props)
                                  :dom.attribute/class   "hf-typeahead"
                                  :dom.property/disabled (::hf/disabled hf/props)
                                  :dom.property/style    {"border-color" (color ~@ hf/db)}
                                  :dom.property/value    (str (if (and label value)
                                                                ~(label value)
                                                                value))
                                  :dom.attribute/list    id}
                           dom/target-value)]
           (when (some? options)
             (binding [hf/args {'needle value'}]
               (let [options ~options]
                 (dom/element "datalist"
                              (dom/attribute "id" id)
                              (dom/attribute "data-count" (count options))
                              (p/for [option options]
                                ;; FIXME dom nodes not unmounting here
                                (dom/option #_(when (= ~hf/value option)
                                                (dom/attribute "selected" true))
                                            (dom/text ((or label identity) option))))))))
           value')))))

(p/defn render-inputs []
  (when (seq hf/inputs)
    (do (log/info "RENDER INPUTS")
      (let [f (first hf/attribute)]
        (dom/div (dom/class "inputs")
                 (merge hf/args
                        (zipmap (map first hf/inputs)
                                (p/for [[arg props] hf/inputs]
                                  (let [id (str (gensym))]
                                    (dom/element "label" (dom/text (str arg))
                                                 (dom/attribute "for" (str id)))
                                    (binding [hf/props {}#_(assoc props :dom.attribute/id id, :dom.attribute/type (input-types (argument-type f arg)))
                                              hf/db    nil
                                              hf/value (::hf/value props)]
                                      (if (some? (::hf/render props))
                                        ~(::hf/render props)
                                        ~default-renderer)))))))))))

(p/def form-impl*)

(p/def -table-picker-props {::cardinality ::one
                            ::group       nil
                            ::value       nil})

(p/defn render-options []
  (when-let [options (::hf/options hf/props)]
    (dom/element "fieldset"
                 (dom/class "hf-options")
                 (dom/element "legend" (dom/text "::hf/options"))
                 ~table-picker)))

(def foo)

(defn schema-attr [db ?a]
  #?(:clj (do #_(assert (bound? #'hf/*$*) "no db")
              #_(condp = (type db)
                datascript.db.DB (get (:schema db) ?a))
              (get (:schema hf/*$*) ?a))))

(defn cardinality [db ?a]
  (case (:db/cardinality (schema-attr db ?a))
    :db.cardinality/one ::one
    :db.cardinality/many ::many
    ::one))

(p/def string-renderer #'(dom/text (str ~hf/value)))

(p/def default-renderer)
(p/defn default-renderer-impl []
  (log/trace "DEFAULT hf/attribute: " ~@ hf/attribute)
  (if-some [type (and (some? ~@ hf/attribute) ;; FIXME binding unification
                      (:hf/valueType ~@ (schema-attr hf/*$* hf/attribute)))]
    (do
      (log/trace "DEFAULT inferred type" type )
      ;; (dom/text (str ~hf/value))
      (binding [hf/props {} #_(assoc hf/props :dom.attribute/type (input-types (spec/valueType->type type)))]
        ~typeahead))
    (dom/code (dom/class "language-clojure") (dom/text (str ~hf/value)))))

(p/defn form-impl []
  (dom/element "form"
               (dom/style {"border-left-color"  (color ~@ hf/db)})
               (let [value ~hf/value]
                 (p/for [column ~@ hf/columns]
                   (do (log/info "column" column)
                       (dom/div (dom/class "field")
                                (dom/style {"border-left-color" (color ~@ hf/db)})
                                (dom/element "label"
                                             (dom/attribute "title" (spec/parse column))
                                             (dom/text column))
                                ~(get value column)))) ))
  #_~render-options)

(p/defn row-impl []
  (dom/tr
   (let [value ~hf/value]
     (binding [form      form-impl
               form-impl form-impl*]
       (p/for [column ~@ hf/columns] ;; FIXME binding unification
         (dom/td (dom/style {"border-color" (color ~@  hf/db)});; FIXME binding unification
                 ~(get value column)))))))

(p/defn table-impl []
  (binding [hf/args ~render-inputs]
    (dom/table
     (dom/thead
      (dom/tr
       (p/for [col ~@ hf/columns] ;; FIXME binding unification
         (dom/th (dom/style {"background-color" (color ~@ hf/db)}) ;; FIXME binding unification
                 (dom/text (pr-str col))))))
     (dom/tbody
      (binding [form row]
        (p/for [row-renderer (seq ~hf/value)]
          ~row-renderer))))))

(p/defn row-picker-impl []
  (dom/tr
   (let [selected? (dom/td (p/$ input {:dom.attribute/type (case (::cardinality -table-picker-props)
                                                             ::one  "radio"
                                                             ::many "checkbox")
                                       :dom.attribute/name (::group -table-picker-props)
                                       :dom.property/checked (= (::value -table-picker-props) (binding [hf/render hf/sequenceM]
                                                                                                ~hf/render))}
                                dom/target-checked))]
     (let [value ~hf/value]
       (binding [form      form-impl
                 form-impl form-impl*]
         (p/for [column ~@ hf/columns]
           (dom/td (dom/style {"border-color" (color ~@ hf/db)})
                   ~(get value column)))))
     selected?)))

(p/defn options-picker-impl []
  (binding [hf/args ~render-inputs]
    (let [cardinality ~@ (cardinality hf/*$* hf/options-attribute)]
      (dom/table
       (dom/thead
        (dom/tr
         (dom/td)
         (p/for [col ~@ hf/columns]
           (dom/th (dom/style {"background-color" (color ~@ hf/db)})
                   (dom/text (pr-str col))))))
       (dom/tbody
        (binding [form                row-picker
                  -table-picker-props {::cardinality cardinality
                                       ::group       (str (gensym))
                                       ::value       (binding [hf/value  (::value -table-picker-props)
                                                               hf/render hf/sequenceM]
                                                       ~hf/render)}]
          (p/for [row-renderer (seq ~hf/value)]
            ~row-renderer)))))))

(p/defn table-picker-impl []
  (binding [table               options-picker
            -table-picker-props {::value hf/value}]
    (do (log/info "TABLE PICKER" hf/props)
        (when-let [options (::hf/options hf/props)]
          (do (log/info "RENDER OPTIONS" options)
              ~options)))))

(p/def spec-renderer)
(p/defn spec-renderer-impl []
  (let [value ~hf/value]
    (log/info "SPEC RENDER" value)
    (cond (map? value)    ~form
          (vector? value) ~table
          :else           ~default-renderer)))

(p/def render #'~spec-renderer)

(p/defn with-spec-render [>cont]
  (binding [form             form-impl
            table            table-impl
            row              row-impl
            table-picker     table-picker-impl
            row-picker       row-picker-impl
            options-picker   options-picker-impl
            boolean          boolean-impl
            default-renderer default-renderer-impl
            typeahead        typeahead-impl
            spec-renderer    spec-renderer-impl]
    ~>cont))

(def exports (p/vars nil? prn some? schema-attr cardinality))
