(ns hyperfiddle.hfql2.ui
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.logger :as log]
            [hyperfiddle.photon-ui :as ui]
            [clojure.datafy :refer [datafy]])
  #?(:cljs (:require-macros [hyperfiddle.hfql2.ui])))

(defn replate-state! [!route path value]
  (swap! !route (fn [[current & history]]
                  (cons (hf/assoc-in-route-state (or current {}) path value) history))))

(defn attr-spec [attr]
  (cond
    (ident? attr) attr
    (seq? attr)   (attr-spec (first attr))))

(p/defn Inputs-renderer [props]
  (when-some [arguments (seq (::hf/arguments props))]
    (let [spec (attr-spec (::hf/attribute props))]
      (p/for [[name {:keys [::hf/read ::hf/write ::hf/path]}] arguments]
        (let [writable? (some? write)
              writef    #(reset! write %)
              value     (read.)]
          (p/client
            (let [id      (random-uuid)
                  !steady (atom false)]
              (dom/label {::dom/for   id
                          ::dom/title (::spec/form (spec/arg spec name))} (dom/text name))
              (ui/input {::dom/id         id
                         ::ui/value       (if (p/watch !steady) (p/current value) value)
                         ::dom/disabled   (not writable?)
                         ::ui/input-event (p/fn [e] (let [value (.. e -target -value)]
                                                      (replate-state! hf/!route-state path value)
                                                      (p/server (writef value))))
                         ::ui/focus-event (p/fn [e] (reset! !steady true))
                         ::ui/blur-event  (p/fn [e] (reset! !steady false))}))))))))

(p/def Table-renderer)
(p/def Form-renderer)
(p/def Row-renderer)

(p/def table-picker-options {::group-id nil, ::current-value nil})

(p/defn Default-options-renderer [V props]
  (when-let [options (::hf/options props)]
    (let [entity hf/entity]
      (p/client
        (dom/fieldset
          (dom/legend (dom/text "Options"))
          (binding [table-picker-options {::group-id (random-uuid), ::current-value entity}]
            (p/server
              (new options))))))))

(p/defn Default-renderer [V props]
  (let [edn (binding [hf/bypass-renderer true] (V.))]
    (p/client
      (dom/pre
        (dom/text edn)))))

(p/defn Form-renderer-impl [V props]
  (p/client
    (dom/form
      (p/server
        (let [data (V.)]
          (p/for [k (::hf/columns props)]
            (p/client
              (dom/label {::dom/title (pr-str (::spec/description (datafy (spec/spec (attr-spec k)))))}  (dom/text k))
              (p/server (new (get data k)))))))))
  (Default-options-renderer. V props))

(p/defn Row-renderer-impl "A row is a transposed form" [V props]
  (binding [Form-renderer Form-renderer-impl]
    (let [row     (V.)
          columns (::hf/columns props)
          entity  hf/entity]
      (p/client
        (dom/tr
          (when-let [id (::group-id table-picker-options)]
            (dom/td (ui/checkbox {::dom/type :radio
                                  ::dom/name id
                                  ::ui/value (= (::current-value table-picker-options) entity)})))
          (p/for [col columns]
            (dom/td (p/server (new (get row col))))))))))

(p/defn Table-renderer-impl [V props]
  (Inputs-renderer. props)
  (let [columns (::hf/columns props)]
    (p/client
      (dom/table
        (dom/thead
          (dom/tr
            (when (::group-id table-picker-options) (dom/th))
            (p/for [col columns]
              (dom/th (pr-str col))  ; TODO attr info on hover
              )))
        (dom/tbody
          (p/server
            (binding [Form-renderer Row-renderer]
              (p/for [row (V.)]
                (row.)))))))))

;; WIP
(def input-type {:hyperfiddle.spec.type/symbol  "text"
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

;; WIP
(p/defn Input-renderer [V props]
  (let [v          (V.)
        value-type (::value-type props)]
    (p/client
      (ui/input {::ui/value v
                 ::ui/type  (input-type value-type "text")}))))

(defn spec-value-type [attr] ; TODO extract spec for quoted sexpr, TODO support args
  (when (qualified-ident? attr)
    (spec/type-of attr)))

(defn schema-attr [db ?a]
  (log/debug "Query DB schema for attr " ?a)
  #?(:clj (condp = (type db)
            datascript.db.DB (get (:schema db) ?a))))

(defn schema-value-type [db a]
  (let [attr (schema-attr db a)]
    (spec/valueType->type (or (:db/valueType attr) (:hf/valueType attr))))) ; datascript rejects valueType other than ref.

(p/defn Spec-renderer [V props]
  (let [attr       (::hf/attribute props)
        value-type (or (spec-value-type attr) (schema-value-type hf/*$* attr))]
    (case value-type
      :hyperfiddle.spec.type/string (Input-renderer. V (assoc props ::value-type value-type))
      (Default-renderer. V props))))

(p/defn Render [V props]
  (if hf/bypass-renderer
    (hf/Join-all. (V.))
    (if-let [Renderer (::hf/render props)]
      (Renderer. V props)
      (let [Renderer (case (::hf/render-as props)
                       ::hf/form  Form-renderer
                       ::hf/table Table-renderer
                       ::hf/field Spec-renderer
                       Default-renderer)]
        (Renderer. V props)))))

(defmacro with-ui-renderers [& body]
  `(binding [Table-renderer Table-renderer-impl
             Form-renderer  Form-renderer-impl
             Row-renderer   Row-renderer-impl]
     ~@body))
