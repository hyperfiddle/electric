(ns hyperfiddle.ui5
  (:refer-clojure :exclude [boolean])
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.spec :as spec]
            [missionary.core :as m]
            [datascript.db]
            #?(:clj [hyperfiddle.q8 :as hfql])
            #?(:clj [datahike.api :as d])
            [hyperfiddle.logger :as log])
  #?(:cljs (:require-macros [hyperfiddle.q8 :as hfql]
                            [hyperfiddle.ui5 :refer [render
                                                     spec-renderer spec-renderer-impl
                                                     default-renderer default-renderer-impl
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
                                                     typeahead
                                                     with-spec-render
                                                     render-mode-selector
                                                     ]])))

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

(p/def default-renderer)
(p/def table)
(p/def row)
(p/def form)
(p/def table-picker)
(p/def row-picker)
(p/def options-picker)
(p/def boolean)

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

(p/defn typeahead []
  (binding [hf/render hf/sequenceM]
    (let [options     (::hf/options hf/props)
          label       (::hf/option-label hf/props)
          attr-type   (:dom.attribute/type hf/props)
          disabled    (::hf/disabled hf/props)
          c           (color hf/db)
          value       ~hf/value
          input-value (str (if (and label value) ~(label value) value))]
      (log/info "TYPEAHEAD" {:label label, :options options})
      ~@
      (dom/fragment
       (let [id     (str (gensym))
             value' (p/$ input {:dom.attribute/type    attr-type
                                :dom.attribute/class   "hf-typeahead"
                                :dom.property/disabled disabled
                                :dom.property/style    {"border-color" c}
                                :dom.property/value    input-value
                                :dom.attribute/list    id}
                         dom/target-value)]
         ~@ ;; server
         (when-some [options (::hf/options hf/props)]
           (binding [hf/args {'needle value'}]
             (let [options    ~options
                   data-count (count options)
                   ]
               ~@ ;; client
               (dom/element "datalist"
                            (dom/attribute "id" id)
                            (dom/attribute "data-count" data-count)
                            ~@ ;; server
                            (p/for [option options]
                              ~@
                              (dom/option ;; FIXME dom nodes not unmounting here
                               #_(when (= ~hf/value option)
                                   (dom/attribute "selected" true))
                               (dom/text ~@((or label identity) option))))))))
         value')))))

(defn extract-refs [inputs refs]
  (filter second (map (juxt identity (partial get refs)) inputs)))

;; FIXME suboptimal, transfers `args` to client for each argument change, should use a `p/for`
(p/defn render-inputs []
  (let [inputs hf/inputs
        refs   (into {} (p/for [[input ref] (extract-refs inputs hf/refs)]
                          (log/warn "INPUT REF" input ~ref)
                          [input ~ref]))
        args   (merge hf/args refs)
        attr   hf/attribute] ;; FIXME binding unification
    ~@ (dom/div (dom/class "inputs")
                (merge hf/args
                       (zipmap inputs
                               (p/for [arg inputs]
                                 (let [id (str (gensym))]
                                   (log/info "ARG" arg)
                                   (dom/element "label" (dom/text (str arg))
                                                (dom/attribute "for" (str id)))
                                   (p/$ input {:dom.attribute/id      id,
                                               :dom.attribute/type    (input-types (argument-type (first attr) arg))
                                               :dom.property/value    (get args arg)
                                               :dom.property/disabled (contains? refs arg)}
                                        dom/target-value))))))))

;; TODO remove
(p/def form-impl*)

(p/def -table-picker-props {::cardinality ::one
                            ::group       nil
                            ::value       nil})

(p/defn render-options []
  (when-let [options (::hf/options hf/props)]
    ~@(dom/element "fieldset"
                   (dom/class "hf-options")
                   (dom/element "legend" (dom/text "::hf/options"))
                   ~@~table-picker)))

(defn schema-attr [db ?a]
  (when ?a
    (do (log/debug "Query DB schema for attr " ?a)
      #?(:clj (condp = (type db)
                datascript.db.DB (get (:schema db) ?a)
                datahike.db.DB (or (d/entity hf/*$* ?a)
                                   (do (log/info "Unknown attr" ?a)
                                       nil)))))))

(defn cardinality [db ?a]
  (case (:db/cardinality (schema-attr db ?a))
    :db.cardinality/one ::one
    :db.cardinality/many ::many
    ::one))

(p/defn default-renderer-impl []
  (let [value     ~hf/value
        valueType (:db/valueType (schema-attr hf/*$* hf/attribute))]
    (log/info "DEFAULT valueType" valueType)
    (if (some? valueType)
      (binding [hf/props (assoc hf/props :dom.attribute/type (input-types (spec/valueType->type valueType)))]
        ~typeahead)
      (let [value (pr-str value)]
        ~@
        (dom/code (dom/class "language-clojure") (dom/text value))))))

(p/defn form-impl []
  (let [c (color hf/db)]
    ~@
    (dom/element "form"
                 (dom/style {"border-left-color" c})
                 ~@
                 (let [value ~hf/value]
                   (p/for [column hf/columns]
                     ~@
                     (do (log/info "column" column)
                         (dom/div (dom/class "field")
                                  (dom/style {"border-left-color" c})
                                  (dom/element "label"
                                               (dom/attribute "title" (spec/parse column))
                                               (dom/text column))
                                  ~@~(get value column))))) ))
  ~render-options)

(p/defn row-impl []
  ;; server
  (binding [form form-impl] ;; restore binding
    ~@ ;; client
    (dom/tr
     ~@ ;; server
     (let [c     (color hf/db)
           value ~hf/value]
       (p/for [col hf/columns]
         ~@ ;; client
         (dom/td (dom/style {"border-color" c})
                 ~@ ;; server
                 ~(get value col)))))))

(p/defn table-impl []
  (let [columns hf/columns
        c       (color hf/db)]
    (binding [hf/args ~render-inputs]
      ~@
      (dom/table
       (dom/thead
        (dom/tr
         ~@
         (p/for [col columns]
           ~@(dom/th (dom/style {"background-color" c}) ;; FIXME binding unification
                     (dom/text (pr-str col))))))
       (dom/tbody
        ~@(binding [form row]
            (p/for [row-renderer (seq ~hf/value)]
              ~row-renderer)))))))

(p/defn row-picker-impl []
  (binding [form      form-impl
            form-impl form-impl*] ;; restore binding
    (let [cardinality (::cardinality -table-picker-props)
          group       (::group -table-picker-props)
          checked?    (= (::value -table-picker-props) (binding [hf/render hf/sequenceM]
                                                         ~hf/render))
          color       (color hf/db)
          value       ~hf/value
          entity      hf/entity
          attribute   hf/attribute]
      ~@
      (dom/tr
       (let [selected? (dom/td (dom/style {"border-color" color})
                               (p/$ input {:dom.attribute/type   (case cardinality
                                                                   ::one  "radio"
                                                                   ::many "checkbox")
                                           :dom.attribute/name   group
                                           :dom.property/checked checked?}
                                    dom/target-checked))]
         (log/info "TX" [entity attribute selected?])
         ~@
         (p/for [column hf/columns]
           ~@
           (dom/td (dom/style {"border-color" color})
                   ~@
                   ~(get value column)))
         selected?)))))

(p/defn options-picker-impl []
  (binding [hf/args ~render-inputs]
    (let [c           (color hf/db)
          cardinality (cardinality hf/*$* hf/options-attribute)]
      ~@
      (dom/table
       (dom/thead
        (dom/tr
         (dom/td)
         ~@
         (p/for [col hf/columns]
           ~@
           (dom/th (dom/style {"background-color" c})
                   (dom/text (pr-str col))))))
       (dom/tbody
        ~@
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
            ;; boolean          boolean-impl
            default-renderer default-renderer-impl
            spec-renderer    spec-renderer-impl]
    ~>cont))

(def exports (p/vars nil? prn some? schema-attr cardinality conj color
                     input-types argument-type spec/valueType->type
                     assoc = gensym merge zipmap
                     extract-refs))
