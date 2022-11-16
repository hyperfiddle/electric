(ns hyperfiddle.hfql2.ui
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.logger :as log]
            [clojure.datafy :refer [datafy]]
            [contrib.ednish :as ednish]
            [contrib.color :as c])
  #?(:cljs (:require-macros [hyperfiddle.hfql2.ui])))

(defn replate-state! [!route path value]
  (swap! !route (fn [[current & history]]
                  (cons (hf/assoc-in-route-state (or current {}) path value) history))))

(defn attr-spec [attr]
  (cond
    (ident? attr) attr
    (seq? attr)   (attr-spec (first attr))))

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

(def input-type {:hyperfiddle.spec.type/symbol  "text"
                 :hyperfiddle.spec.type/uuid    "text"
                 :hyperfiddle.spec.type/uri     "text"
                 :hyperfiddle.spec.type/instant "datetime-local"
                 :hyperfiddle.spec.type/boolean "checkbox"
                 :hyperfiddle.spec.type/string  "text"
                 :hyperfiddle.spec.type/bigdec  "text"
                 :hyperfiddle.spec.type/keyword "text"
                 :hyperfiddle.spec.type/ref     "text"
                 :hyperfiddle.spec.type/float   "number"
                 :hyperfiddle.spec.type/double  "number"
                 :hyperfiddle.spec.type/long    "number"})

;; ----

(p/def table-picker-options {::group-id nil, ::current-value nil}),

(p/defn Default [v props]
  (let [link (when-let [Link (::hf/link props)] (new Link))]
    (p/client
      (if (some? link)
        (dom/a
          {::dom/href (str "#" (ednish/encode-uri link))}
          (dom/event "click" (fn [e] (.preventDefault e) (hf/navigate! link)))
          (dom/text v))
        (dom/pre (dom/text (pr-str v))))))),

(p/defn Input [v props]
  (let [value-type (::value-type props)
        readonly?  (::readonly props)
        tx?        (some? (::hf/tx props))
        ]
    (p/client
      (let [type (input-type value-type "text")]
        (dom/input
          {::dom/type     (input-type value-type "text"),
           ::dom/disabled readonly?}
          (case type
            "checkbox" (dom/props {::dom/checked v})
            (dom/props {::dom/value (cond (inst? v) (.slice (.toISOString v) 0 16)
                                          :else     (str v))}))
          (when tx?
            (let [!v' (atom nil)
                  v'  (p/watch !v')]
              (when v'
                (p/server ((::hf/tx props) v')))
              (dom/event "input" (fn [^js e] (reset! !v' (.. e -target -value))))))))))),

(p/def Render)
(p/defn Render-impl [V]
  (let [{::keys [render cardinality leaf?], :as props} (meta V)]
    (if render
      (render. V)
      (case cardinality
        ::many
        (Table. (V.) props)
        (let [v (V.)]
          (cond
            leaf?       (SpecDispatch. v props)
            (vector? v) (Table. v props)
            (map? v)    (Form. v props)
            :else       (SpecDispatch. v props))))))),

(p/defn Row [V]
  (let [row     (V.)
        columns (::hf/columns (meta V))
        value   (hf/JoinAllTheTree. V)]
    (p/client
      (dom/tr
        (.. dom/node -style (setProperty "--hyperfiddle-hfql-border-color" (c/color hf/db-name)))
        (when-let [id (::group-id table-picker-options)]
          (dom/td
            (dom/input
              {::dom/type    :radio,
               ::dom/name    id,
               ::dom/checked (= (::current-value table-picker-options) value)})))
        (p/for [col columns] (dom/td (p/server (Render. (get row col)))))))))


(p/defn Form [v props]
  (p/client
    (dom/form
      {:style {:border-left-color (c/color hf/db-name)}}
      (p/server
        (p/for [col (::hf/columns props)]
          (p/client
            (dom/div
              {:class "field"}
              (dom/label
                {::dom/title (pr-str (or (spec-description false (attr-spec col))
                                       (p/server (schema-value-type hf/*schema* hf/db col))))}
                (dom/text col))
              (p/server (let [V (get v col)] (GrayInputs. (meta V)) (Render. V))))))
        (Options. v props)))))


(p/defn GrayInput [label? spec props [name {:keys [::hf/read ::hf/path]}]]
  (let [value (read.)]
    (p/client
      (let [id (random-uuid) !steady (atom false)]
        (when label?
          (dom/label {::dom/for   id,
                      ::dom/title (pr-str (:hyperfiddle.spec/form (spec/arg spec name)))}
            (dom/text name)))
        (dom/input {::dom/id    id,
                    ::dom/value (if (p/watch !steady) (p/current value) value)}
          (when (seq props)
            (dom/props props))
          (dom/event "input"
            (fn [e] (hf/replace-route!
                      (hf/assoc-in-route-state hf/route path (.. e -target -value)))))
          (dom/event "focus" (fn [_] (reset! !steady true)))
          (dom/event "blur" (fn [_] (reset! !steady false))))))) )

(p/defn GrayInputs [props]
  (when-some [arguments (seq (::hf/arguments props))]
    (let [spec (attr-spec (::hf/attribute props))]
      (p/for [arg arguments]
        (GrayInput. true spec nil arg))))),

(p/defn SpecDispatch [v props]
  (let [attr              (::hf/attribute props)
        spec-value-type   (spec-value-type attr)
        schema-value-type (schema-value-type hf/*schema* hf/db attr)
        defined-by-spec?  (and spec-value-type (not schema-value-type))
        value-type        (or spec-value-type schema-value-type)]
    (case value-type
      (:hyperfiddle.spec.type/string
       :hyperfiddle.spec.type/instant
       :hyperfiddle.spec.type/boolean) (Input. v (cond-> (assoc props ::value-type value-type)
                                                  defined-by-spec? (assoc ::readonly true)))
      (Default. v props))))

(p/defn Options [v props]
  (when-let [options (::hf/options props)]
    (when-let [continuation (::hf/continuation props)]
      (let [v (hf/JoinAllTheTree. (with-meta (p/fn [] v) props))]
        (p/client
          (dom/fieldset
            (dom/legend (dom/text "Options"))
            (binding [table-picker-options {::group-id      (random-uuid),
                                            ::current-value v}]
              (p/server
                (Table.
                  (p/for [e (new options)] (p/partial 1 continuation e))
                  (meta continuation)))))))))),
(p/def Table)
(p/defn Table-impl [v props]
  (let [columns (::hf/columns props)]
    (Options. v props)
    (p/client
      (dom/table
        (dom/thead
          (dom/tr
            (when (::group-id table-picker-options) (dom/th))
            (p/for [col columns]
              (dom/th {::dom/title (pr-str (or (spec-description true (attr-spec col))
                                             (p/server (schema-value-type hf/*schema* hf/db col)))),
                       ::dom/style {:background-color (c/color hf/db-name)}}
                (pr-str col)))))
        (dom/tbody (p/server (p/for [V v] (Row. V))))))))


;; TODO understand clearly and write down why this is required
(defmacro with-ui-renderers [& body]
  `(binding [Table  Table-impl
             Render Render-impl]
     ~@body))
