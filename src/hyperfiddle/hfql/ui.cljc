(ns hyperfiddle.hfql.ui
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.logger :as log]
            [clojure.datafy :refer [datafy]]
            [contrib.ednish :as ednish]
            [contrib.color :as c])
  #?(:cljs (:require-macros [hyperfiddle.hfql.ui])))

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
              writef    #(reset! write %) ; TODO is a direct write useful when there is a router?
              value     (read.)]
          (p/client
            (let [id      (random-uuid)
                  !steady (atom false)]
              (dom/label {::dom/for   id
                          ::dom/title (pr-str (::spec/form (spec/arg spec name)))}
                (dom/text name))
              (dom/input {::dom/id       id
                          ::dom/value    (if (p/watch !steady) (p/current value) value)
                          ::dom/disabled (not writable?)}
                (dom/event "input" (fn [e] (hf/replace-route! (hf/assoc-in-route-state hf/route path (.. e -target -value)))))
                (dom/event "focus" (fn [_] (reset! !steady true)))
                (dom/event "blur"  (fn [_] (reset! !steady false)))))))))))

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
  (let [edn  (binding [hf/bypass-renderer true] (V.))
        link (when-let [Link (::hf/link props)] (new Link))]
    (p/client
      (if (some? link)
        (dom/a {::dom/href (str "#" (ednish/encode-uri link))}
          (dom/event "click" (fn [e] (.preventDefault e) (hf/navigate! link)))
          (dom/text edn))
        (dom/pre
          (dom/text (pr-str edn)))))))

(defn spec-value-type [attr] ; TODO extract spec for quoted sexpr ; TODO support args
  (when (qualified-ident? attr)
    (spec/type-of attr)))

(defn schema-value-type [schema-f db a]
  (let [attr (schema-f db a)]
    (spec/valueType->type (or (:db/valueType attr) (:hf/valueType attr))))) ; datascript rejects valueType other than ref.

(defn spec-description [prefer-ret? attr]
  (when-let [spec (datafy (spec/spec attr))]
    (if prefer-ret?
      (case (::spec/type spec)
        ::spec/fspec (::spec/ret spec)
        (::spec/description spec))
      (::spec/description spec))))

(p/defn Form-renderer-impl [V props]
  (p/client
    (dom/form {:style {:border-left-color (c/color hf/db-name)}}
      (p/server
        (let [data (V.)]
          (p/for [k (::hf/columns props)]
            (p/client
              (dom/div {:class "field"}
                (dom/label {::dom/title (pr-str (or (spec-description false (attr-spec k))
                                                  (p/server (schema-value-type hf/*schema* hf/db k))))}
                  (dom/text k))
                (p/server (new (get data k))))))))))
  (Default-options-renderer. V props))

(p/defn Row-renderer-impl "A row is a transposed form" [V props]
  (binding [Form-renderer Form-renderer-impl]
    (let [row     (V.)
          columns (::hf/columns props)
          entity  hf/entity]
      (p/client
        (dom/tr
          ;; set border color for all cells in the row. HACK, --var syntax not supported by dom/style.
          (.. dom/node -style (setProperty "--hyperfiddle-hfql-border-color" (c/color hf/db-name)))
          (when-let [id (::group-id table-picker-options)]
            (dom/td (dom/input {::dom/type    :radio
                                ::dom/name    id
                                ::dom/checked (= (::current-value table-picker-options) entity)})))
          (p/for [col columns]
            (dom/td #_{::dom/style {:border-color (c/color hf/db-name)}}
              (p/server (new (get row col))))))))))

(p/defn Table-renderer-impl [V props]
  (Inputs-renderer. props)
  (Default-options-renderer. V props)
  (let [columns (::hf/columns props)]
    (p/client
      (dom/table
        (dom/thead
          (dom/tr
            (when (::group-id table-picker-options) (dom/th))
            (p/for [col columns]
              (dom/th {::dom/title (pr-str (or (spec-description true (attr-spec col))
                                             (p/server (schema-value-type hf/*schema* hf/db col))))
                       ::dom/style {:background-color (c/color hf/db-name) }}
                (pr-str col))  ; TODO attr info on hover
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
        value-type (::value-type props)
        readonly?  (::readonly props)]
    (p/client
      (dom/input {::dom/value    v
                  ::dom/type     (input-type value-type "text")
                  ::dom/disabled readonly?}))))

(p/defn Spec-renderer [V props]
  (let [attr              (::hf/attribute props)
        spec-value-type   (spec-value-type attr)
        schema-value-type (schema-value-type hf/*schema* hf/db attr)
        defined-by-spec?  (and spec-value-type (not schema-value-type))
        value-type        (or spec-value-type schema-value-type)]
    (case value-type
      :hyperfiddle.spec.type/string (Input-renderer. V (cond-> (assoc props ::value-type value-type)
                                                         defined-by-spec? (assoc ::readonly true)))
      (Default-renderer. V props))))

(p/defn EdnRender [V props]
  (if hf/bypass-renderer
    (hf/Join-all. (V.))
    (let [V (p/fn [] (unreduced (V.)))]
      (if-let [Renderer (::hf/render props)]
        (Renderer. V props)
        (let [Renderer (case (::hf/render-as props)
                         ::hf/form  Form-renderer
                         ::hf/table Table-renderer
                         ::hf/field Spec-renderer
                         ::hf/infer (case (hf/*cardinality* hf/*schema* hf/db (::hf/attribute props))
                                      ::hf/one Form-renderer
                                      ::hf/many Table-renderer
                                      Default-renderer)
                         Default-renderer)]
          (Renderer. V props))))))

(p/def Render EdnRender)

;; TODO understand clearly and write down why this is required
(defmacro with-ui-renderers [& body]
  `(binding [Table-renderer Table-renderer-impl
             Form-renderer  Form-renderer-impl
             Row-renderer   Row-renderer-impl
             hf/Render      Render]
     ~@body))
