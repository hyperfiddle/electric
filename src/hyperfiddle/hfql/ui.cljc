(ns hyperfiddle.hfql.ui
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql :as hfql]
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

;; (dom/a
;;   {::dom/href (str "#" (ednish/encode-uri link))}
;;   (dom/event "click" (fn [e] (.preventDefault e) (hf/navigate! link)))
;;   (dom/text value))

(p/defn Default [{::hf/keys [link link-label] :as ctx}]
  (let [route (when link (new link))
        value (hfql/JoinAllTheTree. ctx)]
    (p/client
      (if (some? route)
        (hf/Link. route link-label)
        (dom/pre (dom/text (pr-str value))))))),

(p/defn Input [{::hf/keys [tx Value] :as ctx}]
  (let [value-type (::value-type ctx)
        readonly?  (::readonly ctx)
        tx?        (some? tx)
        v          (Value.)]
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
                (p/server (new tx v')))
              (dom/event "input" (fn [e] (reset! !v' (.. e -target -value))))))))))),

(p/def Render)
;; TODO adapt to new HFQL macroexpansion
(p/defn Render-impl [{::hf/keys [type cardinality render Value] :as ctx}]
  (if render (render. ctx)
    (case type
      ::hf/leaf (SpecDispatch. ctx)
      ::hf/keys (Form. ctx)
      (case cardinality
        ::hf/many (Table. ctx (Value.))
        (let [v (Value.)]
          (cond
            (vector? v) (Table. ctx v)
            (map? v)    (Render. (assoc v ::parent ctx))
            :else       (throw "unreachable" {:v v})))))))

(p/defn Form [{::hf/keys [keys values] :as ctx}]
  (p/client
    (dom/form
      {:style {:border-left-color (c/color hf/db-name)}}
      (p/server
        (p/for-by first [[key ctx] (partition 2 (interleave keys values))]
          (p/client
            (dom/div
              {:class "field"}
              (dom/label
                {::dom/title (pr-str (or (spec-description false (attr-spec key))
                                       (p/server (schema-value-type hf/*schema* hf/db key))))}
                (dom/text key))
              (p/server (GrayInputs. ctx) (Render. ctx)))))
        (Options. (::parent ctx))))))


(p/defn GrayInput [label? spec props [name {:keys [::hf/read ::hf/path ::hf/options]}]]
  (let [value    (read.)
        options? (some? options)]
    (p/client
      (let [id      (random-uuid) !steady (atom false)
            list-id (random-uuid)]
        (when label?
          (dom/label {::dom/for   id,
                      ::dom/title (pr-str (:hyperfiddle.spec/form (spec/arg spec name)))}
            (dom/text name)))
        (when options?
          (dom/datalist {::dom/id list-id}
            (p/server (p/for [x (options.)]
                        (p/client (dom/option (dom/text x)))))))
        (dom/input {::dom/id    id,
                    ::dom/value (if (p/watch !steady) (p/current value) value)}
          (when (seq props)
            (dom/props props))
          (when options?
            (dom/props {::dom/list list-id}))
          (dom/event "input"
            (fn [e] (hf/replace-route!
                      (hf/assoc-in-route-state hf/route path (.. e -target -value)))))
          (dom/event "focus" (fn [_] (reset! !steady true)))
          (dom/event "blur" (fn [_] (reset! !steady false))))))) )

(p/defn GrayInputs [{::hf/keys [attribute arguments]}]
  (when-some [arguments (seq arguments)]
    (let [spec (attr-spec attribute)]
      (p/for [arg arguments]
        (GrayInput. true spec nil arg))))),

(p/defn SpecDispatch [{::hf/keys [attribute] :as ctx}]
  (let [spec-value-type   (spec-value-type attribute)
        schema-value-type (schema-value-type hf/*schema* hf/db attribute)
        defined-by-spec?  (and spec-value-type (not schema-value-type))
        value-type        (or spec-value-type schema-value-type)]
    (case value-type
      (:hyperfiddle.spec.type/string
       :hyperfiddle.spec.type/instant
       :hyperfiddle.spec.type/boolean) (Input. (cond-> (assoc ctx ::value-type value-type)
                                                 defined-by-spec? (assoc ::readonly true)))
      (Default. ctx))))

(p/defn Options [{::hf/keys [options continuation] :as ctx}]
  (when (and options continuation)
    (let [v (hfql/JoinAllTheTree. ctx)]
      (p/client
        (dom/fieldset
          (dom/legend (dom/text "Options"))
          (binding [table-picker-options {::group-id      (random-uuid),
                                          ::current-value v}]
            (p/server (Table. (dissoc ctx ::parent ::hf/options) (p/for [e (new options)] (new continuation e))))))))))

(p/defn Row [{::hf/keys [values] :as ctx}]
  (p/client
    (dom/tr
      (.. dom/node -style (setProperty "--hyperfiddle-hfql-border-color" (c/color hf/db-name)))
      (when-let [id (::group-id table-picker-options)]
        (let [value (p/server (hfql/JoinAllTheTree. ctx))]
          (dom/td
            (dom/input
              {::dom/type    :radio,
               ::dom/name    id,
               ::dom/checked (= (::current-value table-picker-options) value)}))))
      (p/server
        (p/for [ctx values] (p/client (dom/td (p/server (Render. ctx)))))))))

(p/def Table)
(p/defn Table-impl [{::hf/keys [keys] :as ctx} value]
  (Options. ctx)
  (p/client
    (dom/table
      (dom/thead
        (dom/tr
          (when (::group-id table-picker-options) (dom/th))
          (p/for [col keys]
            (dom/th {::dom/title (pr-str (or (spec-description true (attr-spec col))
                                           (p/server (schema-value-type hf/*schema* hf/db col)))),
                     ::dom/style {:background-color (c/color hf/db-name)}}
              (pr-str col)))))
      (dom/tbody (p/server (p/for [ctx value] (Row. ctx)))))))


;; TODO understand clearly and write down why this is required
(defmacro with-ui-renderers [& body]
  `(binding [Table  Table-impl
             Render Render-impl]
     ~@body))
