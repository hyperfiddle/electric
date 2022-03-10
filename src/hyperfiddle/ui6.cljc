(ns hyperfiddle.ui6
  (:refer-clojure :exclude [boolean])
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.spec :as spec]
            [missionary.core :as m]
            [datascript.db]
            #?(:clj [hyperfiddle.q9 :as hfql])
            #?(:clj [datahike.api :as d])
            [hyperfiddle.dev.logger :as log]
            [hyperfiddle.color :refer [color]])
  #?(:cljs (:require-macros [hyperfiddle.q9 :as hfql]
                            [hyperfiddle.ui6 :refer [render
                                                     spec-renderer spec-renderer-impl
                                                     user-renderer user-renderer-impl
                                                     default-renderer default-renderer-impl
                                                     link-renderer link-renderer-impl
                                                     form form-impl form-impl*
                                                     table table-impl
                                                     row row-impl
                                                     grid grid-impl
                                                     grid-row grid-row-impl
                                                     table-picker table-picker-impl -table-picker-props
                                                     options-picker options-picker-impl
                                                     row-picker row-picker-impl
                                                     input
                                                     ;; boolean boolean-impl
                                                     render-inputs
                                                     render-options
                                                     typeahead
                                                     with-spec-render
                                                     ;; render-mode-selector
                                                     ]])))

;;;;;;;;;;;;;;;;;
;; UI ELEMENTS ;;
;;;;;;;;;;;;;;;;;

(p/def default-renderer)
(p/def link-renderer)
(p/def table)
(p/def row)
(p/def grid)
(p/def grid-row)
(p/def form)
(p/def table-picker)
(p/def row-picker)
(p/def options-picker)
;; (p/def boolean)

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
  (let [v (:dom.property/value props)]
    (dom/input (p/for [[k v] props]
                 (cond
                   (= :dom.property/style k) (dom/style v)
                   (property? k)             (dom/property (name k) v)
                   :else                     (dom/attribute (name k) v)))
               ~(->> (dom/events dom/parent "input")
                     (m/eduction #_(drop 1) (map extractor))
                     (m/reductions {} v)
                     (m/relieve {})))))

;; (defn set-state! [!atom v] (reset! !atom v))

;; (p/defn render-mode-selector []
;;   (let [state (atom ::hfql/default)]
;;     (dom/div (dom/class "hf-render-mode-selector")
;;              (dom/button (dom/text "default")
;;                          ~(->> (dom/events dom/parent "click")
;;                                (m/eduction (map (constantly ::hfql/default))
;;                                            (map (partial set-state! state)))
;;                                (m/reductions {} nil)
;;                                (m/relieve {})))
;;              (dom/button (dom/text "user")
;;                          ~(->> (dom/events dom/parent "click")
;;                                (m/eduction (map (constantly ::hfql/user))
;;                                            (map (partial set-state! state)))
;;                                (m/reductions {} nil)
;;                                (m/relieve {}))))
;;     ~(m/watch state)))

;; (p/defn tooltip [text & body]
;;   (dom/div (dom/class "tooltip")
;;            (dom/div (dom/class "tooltip-text")
;;                     (dom/text (str label-str)))))

(p/defn typeahead [>v props]
  (binding [hf/render hf/sequenceM]
    (let [options     (::hf/options props)
          label       (::hf/option-label props)
          attr-type   (:dom.attribute/type props "search")
          disabled    (::hf/disabled props)
          c           (color hf/db)
          value       ~>v
          input-value (str (if (and label value) ~(label value) value))
          value'
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
             (when-some [options (::hf/options props)]
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
                                 (dom/text ~@((or label identity) option)))))))
             value'))]
      (doto (p/$ hf/tx value') prn))))

(defn extract-refs [inputs refs]
  (filter second (map (juxt identity (partial get refs)) inputs)))

(defn sort-inputs-by-spec [spec inputs]
  (let [arg-position (into {} (map-indexed (fn [idx arg] [(:name arg) idx]) (spec/args spec)))]
    (sort-by (comp arg-position key) inputs)))

(p/defn render-inputs [attr inputs]
  (let [inputs (sort-inputs-by-spec (first attr) inputs)] ;; FIXME binding unification
    (do (prn "intputs" inputs)
        (log/warn 'RENDER-INPUTS attr inputs)
        ~@(dom/div (dom/class "inputs")
                   ~@(p/for [[arg [>v ?!v]] inputs]
                       (let [locked? (nil? ?!v)
                             set-v!  (if locked? (constantly nil) (partial reset! ?!v))
                             v       (str ~>v)]
                         ;; (log/info "ARG" arg v)
                         (let [v' ~@ (let [id       (str (gensym))
                                           arg-spec (spec/arg (first attr) arg)]
                                       (dom/element "label"
                                                    (dom/attribute "data-tooltip" (cond-> (pr-str (:predicate arg-spec))
                                                                                    locked? (str " — internal reference 🔒")))
                                                    (dom/text (name arg))
                                                    (dom/attribute "for" (str id)))
                                       (p/$ input {:dom.attribute/id      id,
                                                   :dom.attribute/type    (input-types (argument-type (first attr) arg))
                                                   :dom.property/value    (str v)
                                                   :dom.property/disabled locked?}
                                            dom/target-value))]
                           (log/info "ARG" arg v "->" v')
                           (if (= v v')
                             (prn "same as before")
                             (do (prn "new value")
                                 #_(log/warn "setting v -> v'" (pr-str [v v']))
                                 (do (set-v! v')
                                     v'))))))))))

;; TODO remove
(p/def form-impl*)

(p/def -table-picker-props {::cardinality ::one
                            ::group       nil
                            ::value       nil})

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

(p/defn link-renderer-impl [>v props]
  (let [[symbolic >href] (::hf/link props)
        symbolic         (pr-str symbolic)
        href             ~>href
        href-str         (pr-str href)
        v                (pr-str ~>v)]
    ~@(let [click (dom/element "a" (dom/attribute "title" symbolic)
                               (dom/attribute "href" href-str)
                               (dom/text v)
                               ~(->> (dom/events dom/parent "click")
                                     (m/eduction (map dom/stop-event!)
                                                 (map (constantly "click")))
                                     (m/reductions {} nil)
                                     (m/relieve {})))]
        (when click
          (do (log/info "Click!" href hf/route)
            (reset! hf/route href))))))

(p/defn default-renderer-impl [>v props]
  (cond
    (some? (::hf/link props)) (p/$ link-renderer >v props)
    :else
    (let [value       ~>v
          [_>e a _>v] (first hf/context)
          valueType   (:db/valueType (schema-attr hf/*$* a))]
      (log/info "DEFAULT valueType" valueType)
      (if (some? valueType)
        (p/$ typeahead >v (assoc props :dom.attribute/type (input-types (spec/valueType->type valueType))))
        (let [value (pr-str value)]
          ~@
          (dom/code (dom/class "language-clojure") (dom/text value)))))))

(p/defn render-options [>v props]
  ~@(dom/element "fieldset"
                 (dom/class "hf-options")
                 (dom/element "legend" (dom/text "::hf/options"))
                 ~@(p/$ table-picker >v props)))

(p/defn form-impl [>v props]
  (let [c  (color hf/db)
        tx ~@(dom/element "form"
                          (dom/style {"border-left-color" c})
                          ~@
                          (let [value ~>v]
                            (p/for [column (::hf/columns props)]
                              ~@
                              (dom/div (dom/class "field")
                                       (dom/style {"border-left-color" c})
                                       (dom/element "label"
                                                    (dom/attribute "title" (spec/parse column))
                                                    (dom/text column))
                                       ~@(let [[_ a⁻¹ _] (second hf/context)]
                                           (when-let [inputs (get-in props [::hf/inputs a⁻¹ column])]
                                             (p/$ render-inputs column inputs)))
                                       ~@ ~(get value column)))) )]
    (when (::hf/options props)
      (p/$ render-options >v props))
    tx))

(p/defn row-impl [>v _props]
  ;; server
  (binding [form form-impl];; restore binding
    (let [c             (color hf/db)
          value         ~>v
          [_ _ _ props] (second hf/context)]
      ~@ ;; client
      (dom/tr
       ~@ ;; server
       (p/for [col (::hf/columns props)]
         ~@ ;; client
         (dom/td (dom/style {"border-color" c})
                 ~@ ;; server
                 ~(get value col)))))))

(p/defn table-impl [>v props]
  (let [columns (::hf/columns props)
        c       (color hf/db)]
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
          (p/for [row-renderer (seq ~>v)]
            ~row-renderer))))))

(p/defn grid-impl [>v props]
  (let [columns (::hf/columns props)
        numcols (count columns)
        c       (color hf/db)]
    ~@
    (dom/table
     (dom/class "grid")
     (dom/style {"grid-template-columns" (str "repeat("numcols", auto)")})
     ~@
     (p/for [col columns]
       ~@(dom/th (dom/style {"background-color" c}) ;; FIXME binding unification
                 (dom/text (pr-str col))))
     ~@(binding [form grid-row]
         (p/for [row-renderer (seq ~>v)]
           ~row-renderer)))))

(p/defn grid-row-impl [>v _props]
  ;; server
  (binding [form form-impl];; restore binding
    (let [c             (color hf/db)
          value         ~>v
          [_ _ _ props] (second hf/context)]
      (p/for [col (::hf/columns props)]
        ~@ ;; client
        (dom/td (dom/style {"border-color" c})
                ~@ ;; server
                ~(get value col))))))

(p/defn row-picker-impl [>v props]
  (binding [form      form-impl
            form-impl form-impl*] ;; restore binding
    (let [color                 (color hf/db)
          [e⁻¹ a⁻¹ v⁻¹ props⁻¹] (::eav -table-picker-props)
          [>e _ _ _]            (first hf/context)
          e                     ~>e
          cardinality           (cardinality hf/*$* a⁻¹)
          group                 (::group -table-picker-props)
          checked?              (= v⁻¹ (p/$ hf/data >v))
          v                     ~>v
          ]
      (log/info "V V" (list v⁻¹ (p/$ hf/data >v)))
      ~@
      (dom/tr
       (let [selected? (dom/td (dom/style {"border-color" color})
                               (p/$ input {:dom.attribute/type   (case cardinality
                                                                   ::one  "radio"
                                                                   ::many "checkbox")
                                           :dom.attribute/name   group
                                           :dom.property/checked checked?}
                                    dom/target-checked))]
         (do (log/info "TX" [e⁻¹ a⁻¹ e] cardinality )
             ~@
             (p/for [column (::hf/columns props⁻¹)]
               ~@
               (dom/td (dom/style {"border-color" color})
                       ~@
                       ~(get v column)))
             selected?))))))

(p/defn options-picker-impl [>v props]
  ;; (binding [hf/args ~render-inputs])
  (let [c               (color hf/db)
        columns         (::hf/columns props)
        [_ a _ _]       (nth hf/context 0)
        [_ a⁻¹ _ _]     (nth hf/context 1)
        [_ _ _ props⁻²] (nth hf/context 2)]
    ;; (log/info "OPTIONS-PICKER-IMPL" a⁻¹ a props⁻²)
    (do (when-let [inputs (get-in props⁻² [::hf/inputs a⁻¹ a])]
          ;; (log/info "inputs" inputs)
          (p/$ render-inputs a inputs))
        ~@
        (dom/table
         (dom/thead
          (dom/tr
           (dom/td (dom/text ""))
           ~@
           (p/for [col columns]
             ~@
             (dom/th (dom/style {"background-color" c})
                     (dom/text (pr-str col))))))
         (dom/tbody
          ~@
          (let [[>e a >v⁻¹ _] (second hf/context)]
            (binding [form                row-picker
                      -table-picker-props {::eav   [~>e a ~>v⁻¹ props]
                                           ::group (str (gensym))}]
              (p/for [row-renderer (seq ~>v)]
                ~row-renderer))))))))

(p/defn table-picker-impl [>v props]
  (binding [table options-picker]
    ~(::hf/options props)))

(p/def spec-renderer)
(p/defn spec-renderer-impl [>v props]
  (let [value    ~>v
        renderer (cond (map? value)    form
                       (vector? value) table
                       :else           default-renderer)]
    (p/$ renderer >v props)))

(p/def user-renderer)
(p/defn user-renderer-impl [>v props]
  (if-let [renderer (::hf/render props)]
    (p/$ renderer >v props)
    (p/$ spec-renderer >v props)))

(p/def render #'~user-renderer)

(p/defn with-spec-render [>cont]
  (binding [form             form-impl
            table            table-impl
            row              row-impl
            grid             grid-impl
            grid-row         grid-row-impl
            table-picker     table-picker-impl
            row-picker       row-picker-impl
            options-picker   options-picker-impl
            ;; boolean          boolean-impl
            default-renderer default-renderer-impl
            link-renderer    link-renderer-impl
            spec-renderer    spec-renderer-impl
            user-renderer    user-renderer-impl
            ]
    ~>cont))

(def exports (p/vars nil? prn some? schema-attr cardinality conj color
                     input-types argument-type spec/valueType->type
                     assoc = gensym merge zipmap
                     extract-refs
                     sort-inputs-by-spec))
