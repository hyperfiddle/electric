(ns hyperfiddle.ui
  (:refer-clojure :exclude [boolean])
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom3 :as dom]
            [hyperfiddle.spec :as spec]
            [missionary.core :as m]
            [datascript.db]
            #?(:clj [datahike.api :as d])
            [hyperfiddle.logger :as log]
            [hyperfiddle.ui.color :refer [color]])
  #?(:cljs (:require-macros [hyperfiddle.ui :refer [link]])))

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

(defn adapt-checkbox-props [props]
  (if (= "checkbox" (:dom.attribute/type props))
    (-> props
        (assoc :dom.property/checked (:dom.property/value props))
        (dissoc :dom.property/value))
    props))

(p/defn Input "if value prop, controlled else uncontrolled" [props extractor]
  (dom/input (dom/props (adapt-checkbox-props props))
             (new (dom/events "input" (map extractor)))))

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
  (binding [hf/Render (p/fn [V _props] (hf/Data. V))]
    (let [options     (::hf/options props)
          label       (::hf/option-label props)
          attr-type   (:dom.attribute/type props "search")
          disabled    (::hf/disabled props)
          c           (db-color hf/db)
          value       (V.)
          input-value (str (if (and label value) (new (label value)) value))
          value'
          ~@(dom/element "fragment"
             (let [id     (str (gensym))
                   value' (Input. {:dom.attribute/type    attr-type
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
                       (dom/datalist {:id         id
                                      :data-count data-count}
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
  (binding [hf/Render (p/fn [V _props] (hf/Data. V))]
    (let [label    (::hf/option-label props)
          disabled (::hf/disabled props)
          c        (db-color hf/db)
          value    (hf/Data. V)
          ;; input-value (str (if (and label value) ~(label value) value))
          value'
          ~@(let [value' (dom/select {:class    "hf-select"
                                      :disabled disabled
                                      :style    {:border-color c}}
                                     ~@;; server
                                       (when-some [Options (::hf/options props)]
                                         (let [options (Options.)
                                               index   (index-by index-id options)]
                                           (do
                                             (p/for [option options]
                                               (let [selected? (= value option)]
                                                 ~@(dom/option {:selected selected? ;; FIXME dom nodes not unmounting here
                                                                :value ~@(index-id option)}  ;; index-id might be platform-specific
                                                    (dom/text ~@((or label identity) option)))))
                                             ~@(new  (dom/events "input" (comp (map dom/target-value)
                                                                               (map index))))))))]
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
        ~@(dom/div {:class "inputs"}
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
                                            (dom/label {:for (str id)
                                                        :data-tooltip
                                                        (cond-> (pr-str (:predicate arg-spec))
                                                          locked? (str " — internal reference 🔒"))}
                                                       (dom/text (name arg)))
                                            (let [v' (Input. {:id      id,
                                                              :type    (input-types (argument-type (first attr) arg))
                                                              :value    v
                                                              :disabled locked?}
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

(defmacro link [href on-click & body] ;; GG: TODO should it be a p/def or a macro?
  (prn "link" href body)
  `(dom/a {:href (str ~href)}
          (new (dom/events "click" (comp (map dom/stop-event!)
                                         (map ~on-click)
                                         (map (constantly nil))
                                         (dedupe))) ; don't propagate `nil` on every click
               )
          ~@body))

(p/def link-renderer)
(p/defn link-renderer-impl [V props]
  (let [[symbolic Href] (::hf/link props)
        symbolic         (pr-str symbolic)
        href             (Href.)
        href-str         (pr-str href)
        v                (pr-str (V.))]
    ~@(link href-str (partial set-route! href)
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
          ~@(do (dom/code {:class "language-clojure"} (dom/text value))
                nil ;; no tx
                ))))))

(p/defn render-options [V props]
  ~@(dom/fieldset {:class "hf-options"}
      (dom/legend (dom/text "::hf/options"))
      ~@(table-picker. V props)))

(defn into-tx [txs] (into [] cat txs))

(p/def form)
(p/defn form-impl [V props]
  (let [c  (db-color hf/db)
        tx ~@(dom/form
              {:style {"border-left-color" c}}
              ~@(let [value (V.)]
                  (into-tx
                   (p/for [column (::hf/columns props)]
                     ~@(dom/div {:class "field"
                                 :style {"border-left-color" c}}
                                (dom/label {:title (spec/parse column)}
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
                      (dom/td {:style {"border-color" c}}
                              ~@;; server
                              (new (get value col)))))))))

(p/defn table-impl [V props]
  (let [columns (::hf/columns props)
        c       (db-color hf/db)]
    ~@(dom/table
       (dom/thead
        (dom/tr
         ~@(p/for [col columns]
             ~@(dom/th {:style {"background-color" c}} ;; FIXME binding unification
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
       {:class "grid"
        :style {"grid-template-columns" (str "repeat(" numcols ", auto)")}}
       ~@(p/for [col columns]
           ~@(dom/th {:style {"background-color" c}} ;; FIXME binding unification
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
        (dom/td {:style {"border-color" c}}
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
          checked?              (= v⁻¹ (hf/Data. V))
          v                     (V.)]
      (log/info "V V" (list v⁻¹ (hf/Data. V)))
      ~@(binding [dom/parent (do e dom/parent)]
          (dom/tr
           (let [selected? (dom/td {:style {"border-color" color}}
                                   (Input. {:type   (case cardinality
                                                      ::one  "radio"
                                                      ::many "checkbox")
                                            :name   group
                                            :checked checked?}
                                     dom/target-checked))]
             (do (log/info "TX" [e⁻¹ a⁻¹ e] cardinality)
                 ~@(p/for [column (::hf/columns props⁻¹)]
                     ~@(dom/td {:style {"border-color" color}}
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
         (dom/td {:class "hf-table-picker-count"} (dom/text "" #_(str v-count)))
         ~@(p/for [col columns]
             ~@(dom/th {:style {"background-color" c}}
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
        Renderer (p/deduping (cond (map? value)    form
                               (vector? value) table
                               :else           default-renderer))]
    (Renderer. V props)))

(p/def user-renderer)
(p/defn user-renderer-impl [V props]
  (if-let [Renderer (::hf/render props)]
    (Renderer. V props)
    (spec-renderer. V props)))

(p/defn Render [] (user-renderer.))

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
