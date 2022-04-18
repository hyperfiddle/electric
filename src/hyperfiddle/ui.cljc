(ns hyperfiddle.ui
  (:refer-clojure :exclude [boolean])
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-xp :as xp]
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
                            [hyperfiddle.photon-xp :as xp]
                            [hyperfiddle.ui :refer [render
                                                     spec-renderer spec-renderer-impl
                                                     user-renderer user-renderer-impl
                                                     default-renderer default-renderer-impl
                                                     link link-renderer link-renderer-impl
                                                     form form-impl
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
                                                     select-options select-options-impl
                                                     with-spec-render
                                                     ;; render-mode-selector
                                                     ]])))

;;;;;;;;;;;;;;;;;
;; UI ELEMENTS ;;
;;;;;;;;;;;;;;;;;

(p/def table)
(p/def row)
(p/def grid)
(p/def grid-row)

(p/def table-picker)

;; (p/def boolean)

(defn input-types [type]
  (get {:hyperfiddle.spec.type/symbol  "text"
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
        :hyperfiddle.spec.type/long    "number"}
       type "text"))

(defn argument-type [f arg] (spec/type-of f arg))

(defn property? [kw] (= "dom.property" (namespace kw)))

(defn debounce [delay flow]
  (m/ap (let [x (m/?< flow)]
          (try (m/? (m/sleep delay x))
               (catch #?(:clj Exception, :cljs :default) _ (m/?> m/none))))))

(defn ^:deprecated continuous [& args] (apply hyperfiddle.photon-xp/continuous args))

(defn adapt-checkbox-props [props]
  (if (= "checkbox" (:dom.attribute/type props))
    (-> props
        (assoc :dom.property/checked (:dom.property/value props))
        (dissoc :dom.property/value))
    props))

(p/defn input [props extractor]
  (dom/input (p/for [[k v] (adapt-checkbox-props props)]
               (cond
                 (= :dom.property/style k) (dom/style v)
                 (property? k)             (dom/property (name k) v)
                 :else                     (dom/attribute (name k) v)))
    (new (->> (dom/events dom/parent "input")
           (m/eduction (map extractor))
           (continuous)))))

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

(defn db-color [db] (color (str (:name db) (* 100 (:basis-t db)))))

(p/defn typeahead [V props]
  (binding [hf/render hf/sequenceM]
    (let [options     (::hf/options props)
          label       (::hf/option-label props)
          attr-type   (:dom.attribute/type props "search")
          disabled    (::hf/disabled props)
          c           (db-color hf/db)
          value       (V.)
          input-value (str (if (and label value) (new (label value)) value))
          value'
          ~@(dom/fragment
             (let [id     (str (gensym))
                   value' (input. {:dom.attribute/type    attr-type
                                   :dom.attribute/class   "hf-typeahead"
                                   :dom.property/disabled disabled
                                   :dom.property/style    {"border-color" c}
                                   :dom.property/value    input-value
                                   :dom.attribute/list    id}
                            dom/target-value)]
               ~@;; server
                 (when-some [Options (::hf/options props)]
                   (let [options (Options.)
                         data-count (count options)]
                     ~@;; client
                       (dom/element "datalist"
                                    (dom/attribute "id" id)
                                    (dom/attribute "data-count" data-count)
                                    ~@;; server
                                      (p/for [option options]
                                        ~@(dom/option ;; FIXME dom nodes not unmounting here
                                           #_(when (= ~hf/value option)
                                               (dom/attribute "selected" true))
                                           (dom/text ~@((or label identity) option)))))))
               value'))]
      (hf/tx. value' props))))

(defn index-by [kf coll] (into {} (map (juxt kf identity)) coll))
(defn index-id [x] (str (hash x)))

(p/def select-options)
(p/defn select-options-impl [V props]
  (binding [hf/render hf/sequenceM]
    (let [label    (::hf/option-label props)
          disabled (::hf/disabled props)
          c        (db-color hf/db)
          value    (hf/data. V)
          ;; input-value (str (if (and label value) ~(label value) value))
          value'
          ~@(let [value' (dom/select (dom/class "hf-select")
                                     (dom/property "disabled" disabled)
                                     (dom/style {"border-color" c})
                                     ~@;; server
                                       (when-some [Options (::hf/options props)]
                                         (let [options (Options.)
                                               index   (index-by index-id options)]
                                           (do
                                             (p/for [option options]
                                               (let [selected? (= value option)]
                                                 ~@(dom/option ;; FIXME dom nodes not unmounting here
                                                    (when selected?
                                                      (dom/attribute "selected" "selected"))
                                                    (dom/attribute "value" ~@(index-id option)) ;; index-id might be platform-specific
                                                    (dom/text ~@((or label identity) option)))))
                                             ~@(new (->> (dom/events dom/parent "input")
                                                      (m/eduction (map dom/target-value)
                                                        (map index))
                                                      (continuous)))))))]
              value')]
      (hf/tx. value' props))))

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
                   ~@(p/for [[idx [arg [V ?!v]]] (map-indexed vector inputs)]
                       (let [locked? (nil? ?!v)
                             set-v!  (if locked? (constantly nil) (partial reset! ?!v))
                             v       (V.)]
                         (when-some [v' ~@(let [id         (str (gensym))
                                                arg-spec   (spec/arg (first attr) arg)
                                                input-type (input-types (argument-type (first attr) arg))
                                                extractor  (if (= "checkbox" input-type)
                                                             dom/target-checked
                                                             dom/target-value)]
                                            (dom/element "label"
                                                         (dom/attribute "data-tooltip" (cond-> (pr-str (:predicate arg-spec))
                                                                                         locked? (str " — internal reference 🔒")))
                                                         (dom/text (name arg))
                                                         (dom/attribute "for" (str id)))
                                            (let [v' (input. {:dom.attribute/id      id,
                                                              :dom.attribute/type    (input-types (argument-type (first attr) arg))
                                                              :dom.property/value    v
                                                              :dom.property/disabled locked?}
                                                       extractor)]
                                              (log/info "extracted" v')
                                              v'))]
                           (log/info "ARG" arg v "->" v')
                           (if (= v v')
                             (prn "same as before")
                             (do (prn "new value")
                                 ~@(hf/set-route-arg! (inc idx) v')
                                 (set-v! v')
                                 )))))))))

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

(defn set-route! [href _event] (hf/navigate! href))

(defmacro link [href on-click & body]
  `(xp/forget (dom/element "a" (dom/attribute "href" (str ~href))
                        (unquote (->> (dom/events dom/parent "click")
                                      (m/eduction (map dom/stop-event!)
                                                  (map ~on-click)
                                                  (map (constantly nil)))
                                      (m/reductions {} nil)
                                      (m/relieve {})))
                        ~@body)))

(p/def link-renderer)
(p/defn link-renderer-impl [V props]
  (let [[symbolic Href] (::hf/link props)
        symbolic         (pr-str symbolic)
        href             (Href.)
        href-str         (pr-str href)
        v                (pr-str (V.))]
    ~@(link href-str (partial set-route! href)
            (dom/attribute "href" href-str)
            (dom/text v))))

(p/def default-renderer)
(p/defn default-renderer-impl [V props]
  (cond
    (some? (::hf/link props)) (link-renderer. V props)
    :else
    (let [value       (V.)
          [_>e a _>v] (first hf/context)
          valueType   (:db/valueType (schema-attr hf/*$* a))]
      (log/info "DEFAULT valueType" valueType)
      (if (some? valueType)
        (typeahead. V (assoc props :dom.attribute/type (input-types (spec/valueType->type valueType))))
        (let [value (pr-str value)]
          ~@(do (dom/code (dom/class "language-clojure") (dom/text value))
                nil ;; no tx
                ))))))

(p/defn render-options [V props]
  ~@(dom/element "fieldset"
                 (dom/class "hf-options")
                 (dom/element "legend" (dom/text "::hf/options"))
                 ~@(table-picker. V props)))

(defn into-tx [txs] (into [] cat txs))

(p/def form)
(p/defn form-impl [V props]
  (let [c  (db-color hf/db)
        tx ~@(dom/element "form"
                          (dom/style {"border-left-color" c})
                          ~@(let [value (V.)]
                              (into-tx
                               (p/for [column (::hf/columns props)]
                                 ~@(dom/div (dom/class "field")
                                            (dom/style {"border-left-color" c})
                                            (dom/element "label"
                                                         (dom/attribute "title" (spec/parse column))
                                                         (dom/text column))
                                            ~@(do
                                                (let [[_ a⁻¹ _] (second hf/context)]
                                                  (when-let [inputs (get-in props [::hf/inputs a⁻¹ column])]
                                                    (render-inputs. column inputs)))
                                                (new (get value column))))))))]
    (when (::hf/options props)
      (render-options. V props))
    tx))

(p/defn row-impl [V _props]
  ;; server
  (binding [form form-impl];; restore binding
    (let [c             (db-color hf/db)
          value         (V.)
          [_ _ _ props] (second hf/context)]
      ~@;; client
        (dom/tr
         ~@;; server
           (into-tx (p/for [col (::hf/columns props)]
                      ~@;; client
                        (dom/td (dom/style {"border-color" c})
                                ~@;; server
                                    (new (get value col)))))))))

(p/defn table-impl [V props]
  (let [columns (::hf/columns props)
        c       (db-color hf/db)]
    ~@(dom/table
       (dom/thead
        (dom/tr
         ~@(p/for [col columns]
             ~@(dom/th (dom/style {"background-color" c}) ;; FIXME binding unification
                       (dom/text (pr-str col))))))
       (dom/tbody
        ~@(binding [form row]
            (into-tx
             (p/for [RowRenderer (seq (V.))]
               (RowRenderer.))))))))

(p/defn grid-impl [V props]
  (let [columns (::hf/columns props)
        numcols (count columns)
        c       (db-color hf/db)]
    ~@(dom/table
       (dom/class "grid")
       (dom/style {"grid-template-columns" (str "repeat(" numcols ", auto)")})
       ~@(p/for [col columns]
           ~@(dom/th (dom/style {"background-color" c}) ;; FIXME binding unification
                     (dom/text (pr-str col))))
       ~@(binding [form grid-row]
           (p/for [RowRenderer (seq (V.))]
             (RowRenderer.))))))

(p/defn grid-row-impl [V _props]
  ;; server
  (binding [form form-impl];; restore binding
    (let [c             (db-color hf/db)
          value         (V.)
          [_ _ _ props] (second hf/context)]
      (p/for [col (::hf/columns props)]
        ~@;; client
          (dom/td (dom/style {"border-color" c})
                  ~@;; server
                      (new (get value col)))))))

(p/def row-picker)
(p/defn row-picker-impl [V props]
  (binding [form      form-impl] ;; restore binding
    (let [color                 (db-color hf/db)
          [e⁻¹ a⁻¹ v⁻¹ props⁻¹] (::eav -table-picker-props)
          [E _ _ _]             (first hf/context)
          e                     (E.)
          cardinality           (cardinality hf/*$* a⁻¹)
          group                 (::group -table-picker-props)
          checked?              (= v⁻¹ (hf/data. V))
          v                     (V.)]
      (log/info "V V" (list v⁻¹ (hf/data. V)))
      ~@(binding [dom/parent (do e dom/parent)]
          (dom/tr
           (let [selected? (dom/td (dom/style {"border-color" color})
                                   (input. {:dom.attribute/type   (case cardinality
                                                                    ::one  "radio"
                                                                    ::many "checkbox")
                                            :dom.attribute/name   group
                                            :dom.property/checked checked?}
                                     dom/target-checked))]
             (do (log/info "TX" [e⁻¹ a⁻¹ e] cardinality)
                 ~@(p/for [column (::hf/columns props⁻¹)]
                     ~@(dom/td (dom/style {"border-color" color})
                               ~@(new (get v column))))
                 selected?)))))))

(p/def options-picker)
(p/defn options-picker-impl [V props]
  (let [c               (db-color hf/db)
        columns         (::hf/columns props)
        [_ a _ _]       (nth hf/context 0)
        [_ a⁻¹ _ _]     (nth hf/context 1)
        [_ _ _ props⁻²] (nth hf/context 2)
        v               (V.)
        v-count (count v) ]
    (when-let [inputs (get-in props⁻² [::hf/inputs a⁻¹ a])]
      (render-inputs. a inputs))
    ~@(dom/table
       (dom/thead
        (dom/tr
         (dom/td (dom/class "hf-table-picker-count") (dom/text "" #_(str v-count)))
         ~@(p/for [col columns]
             ~@(dom/th (dom/style {"background-color" c})
                       (dom/text (pr-str col))))))
       (dom/tbody
        ~@(let [[E a V⁻¹ _] (second hf/context)]
            (binding [form                row-picker
                      -table-picker-props {::eav   [(E.) a (V⁻¹.) props]
                                           ::group (str (gensym))}]
              (p/for [RowRenderer v]
                (RowRenderer.))))))))

(p/defn table-picker-impl [V props]
  (binding [table options-picker]
    (new (::hf/options props))))

(p/def spec-renderer)
(p/defn spec-renderer-impl [V props]
  (let [value (V.)
        Renderer (xp/deduping (cond (map? value)    form
                                 (vector? value) table
                                 :else           default-renderer))]
    (Renderer. V props)))

(p/def user-renderer)
(p/defn user-renderer-impl [V props]
  (if-let [Renderer (::hf/render props)]
    (Renderer. V props)
    (spec-renderer. V props)))

(p/defn render [] (user-renderer.))

(defmacro with-spec-render [& body]
  `(binding [form             form-impl
             table            table-impl
             row              row-impl
             grid             grid-impl
             grid-row         grid-row-impl
             table-picker     table-picker-impl
             row-picker       row-picker-impl
             options-picker   options-picker-impl
             select-options   select-options-impl
            ;; boolean          boolean-impl
             default-renderer default-renderer-impl
             link-renderer    link-renderer-impl
             spec-renderer    spec-renderer-impl
             user-renderer    user-renderer-impl]
     ~@body))

(def exports (p/vars nil? prn some? schema-attr cardinality conj db-color
                     input-types argument-type spec/valueType->type
                     assoc = gensym merge zipmap
                     extract-refs
                     sort-inputs-by-spec
                     into-tx
                     index-by
                     index-id))
