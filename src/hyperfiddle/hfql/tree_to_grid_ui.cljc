(ns hyperfiddle.hfql.tree-to-grid-ui
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.hfql :as hfql]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.spec :as spec]
            [clojure.datafy :refer [datafy]]
            ;; [contrib.ednish :as ednish]
            [contrib.color :as c]
            [contrib.data :as data]
            [hyperfiddle.photon-ui2 :as ui2]
            [hyperfiddle.scrollview :as sw]
            #?(:clj [hyperfiddle.txn :as txn]))
  #?(:cljs (:require-macros [hyperfiddle.hfql.tree-to-grid-ui])))

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

(p/def grid-width 2) ; TODO infer from ctx

(p/def grid-row 1) ; TODO not called with new, don’t capitalize
(p/def grid-col 1)
(p/def indentation 0)
(p/def pagination-offset)

(defmacro cell [row col & body]
  `(dom/div {::dom/role  "cell"
             ::dom/style {:grid-column ~col
                          :grid-row    ~row}}
     ~@body))

(defn find-best-identity [v] ; TODO look up in schema
  (cond (map? v) (or (:db/ident v) (:db/id v))
        :else    v))

(p/defn Identity [x] x)

(p/defn Default [{::hf/keys [entity link link-label options option-label continuation tx] :as ctx}]
  (let [route        (when link (new link))
        value        (hfql/JoinAllTheTree. ctx)
        options      (or options (::hf/options (::parent ctx)))
        option-label (or option-label (::hf/option-label (::parent ctx)) Identity)
        continuation (or continuation (::hf/continuation (::parent ctx)))
        tx           (or tx (::hf/tx (::parent ctx)))
        tx?          (some? tx)
        dom-props    (data/select-ns :hyperfiddle.photon-dom ctx)]
    (cond
      (some? route)   (p/client (cell grid-row grid-col (hf/Link. route link-label)))
      (some? options) (let [value (find-best-identity value)]
                        (p/client
                          (let [v' (ui2/select (p/server (p/for [e (options.)]
                                                           (let [v (if continuation (hfql/JoinAllTheTree. (new continuation e)) e)]
                                                             {:text  (new option-label v)
                                                              :value (find-best-identity v)})))
                                     value
                                     (dom/props {::dom/role     "cell"
                                                 ::dom/style    {:grid-row grid-row, :grid-column grid-col}
                                                 ::dom/disabled (not tx?)})
                                     (dom/props dom-props))]
                            (when (and tx? (not= value v'))
                              (p/server
                                (let [ctx (if (::hf/tx ctx) ctx (::parent ctx))]
                                  (when tx (tx. ctx v'))))))))
      :else
      (p/client
        (dom/pre {::dom/role  "cell"
                  ::dom/style {:grid-row grid-row, :grid-column grid-col}}
          (dom/text
            (str (non-breaking-padder indentation)
              (pr-str value)))))))),

(p/defn Input [{::hf/keys [tx Value] :as ctx}]
  (let [value-type (::value-type ctx)
        readonly?  (::readonly ctx)
        tx?        (some? tx)
        v          (Value.)
        dom-for    (::dom/for ctx)]
    (p/client
      (let [type (input-type value-type "text")]
        (dom/input
          {::dom/role     "cell"
           ::dom/type     type,
           ::dom/disabled readonly?
           ::dom/style    {:grid-row grid-row, :grid-column grid-col}}
          (case type
            "checkbox" (dom/props {::dom/checked v})
            (dom/props {::dom/value (cond (inst? v) (.slice (.toISOString v) 0 16)
                                          :else     (str v))}))
          (when dom-for
            (dom/props {::dom/id dom-for}))
          (when tx?
            (let [!v' (atom nil)
                  v'  (p/watch !v')]
              (dom/event "input" (fn [e] (reset! !v' (.. e -target -value))))
              (when v'
                (p/server (tx. ctx v')))))))))),

(defn give-card-n-contexts-a-unique-key [ctxs]
  (into [] (map-indexed (fn [idx ctx] (assoc ctx ::key idx))) ctxs))

(p/def Render)

;; This should not be see in userland because it’s an implementation detail
;; driven by Photon not supporting mutual recursion as of today.
(defmacro with-gridsheet-renderer [& body]
  `(p/server
     (binding [Table     Table-impl
               Render    Render-impl
               hf/Render Render-impl]
       (p/client ; FIXME don’t force body to run on the client
         (dom/div {:class "hyperfiddle-gridsheet"} ; FIXME drop the wrapper div
           ~@body)))))

;; TODO adapt to new HFQL macroexpansion
(p/defn Render-impl [{::hf/keys [type cardinality render Value] :as ctx}]
  (if render (render. ctx)
      (case type
        ::hf/leaf (SpecDispatch. ctx)
        ::hf/keys (Form. ctx)
        (case cardinality
          ::hf/many (Table. ctx (give-card-n-contexts-a-unique-key (Value.)))
          (let [v (Value.)]
            (cond
              (vector? v) (Table. ctx (give-card-n-contexts-a-unique-key v))
              (map? v)    (Render. (assoc v ::parent ctx))
              :else       (throw "unreachable" {:v v})))))))

(defn height [ctx]
  (cond
    (::hf/height ctx)  (+ (::hf/height ctx)
                        (count (::hf/arguments ctx)))
    (::hf/options ctx) 1
    (::hf/keys ctx)    (+ (count (::hf/arguments ctx)) (count (::hf/keys ctx)))
    ;; TODO handle unknown height
    :else              1))

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

(defn non-breaking-padder [n] (apply str (repeat n " ")) )


(defn field-name [attr]
  (if (seq? attr)
    (cons (symbol (field-name (first attr))) (seq (::spec/keys (clojure.datafy/datafy (spec/args (first attr))))) )
    (name attr)))

(p/defn GrayInput [label? spec props [name {:keys [::hf/read ::hf/path ::hf/options ::hf/option-label] :as arg}]]
  (let [value    (read.)
        options? (some? options)]
    (p/client
      (let [id       (random-uuid) !steady (atom false)
            list-id  (random-uuid)
            arg-spec (spec/arg spec name)]
        (when label?
          (dom/label {::dom/role  "cell"
                      ::dom/class "label"
                      ::dom/for   id,
                      ::dom/title (pr-str (:hyperfiddle.spec/form arg-spec))
                      ::dom/style {:grid-row    grid-row
                                   :grid-column grid-col
                                   :color :gray}}
            (dom/text (str (non-breaking-padder indentation) (field-name  name)))))
        (when options?
          (dom/datalist {::dom/id list-id}
            (p/server (let [labelf (or option-label (p/fn [x] x))]
                        (p/for [x (options.)]
                          (let [text (labelf. x)]
                            (p/client (dom/option (dom/text text)))))))))

        (let [type  (spec/type-of spec name)
              value (if (p/watch !steady) (p/current value) value)
              v     (atom nil)]
          (dom/input {::dom/id    id
                      ::dom/role  "cell"
                      ::dom/type  (input-type type)
                      ::dom/style {:grid-row    grid-row
                                   :grid-column (inc grid-col)}}

            (case type
              :hyperfiddle.spec.type/boolean (dom/props {::dom/checked value})
              (dom/props {::dom/value value}))
            (when (seq props)
              (dom/props props))
            (when options?
              (dom/props {::dom/list list-id}))
            (let [route hf/route]
              (dom/event "input"
                (fn [e]
                  (reset! v (.. e -target -value))
                  (hf/replace-route!
                    (hf/assoc-in-route-state route path (.. e -target -value))))))
            (dom/event "focus" (fn [_] (reset! !steady true)))
            (dom/event "blur" (fn [_] (reset! !steady false))))
          (p/watch v))))) )

(p/defn CellPad [row col-offset]
  (let [n (- grid-width grid-col (dec col-offset))]
    (p/for [i (range n)]
      (cell row (+ grid-col (dec col-offset) (inc i))))))

(p/defn GrayInputs [{::hf/keys [attribute arguments]}]
  (if-some [arguments (seq arguments)]
    (let [spec (attr-spec attribute)]
      (p/for-by second [[idx arg] (map-indexed vector arguments)]
        (p/client
          (binding [grid-row (+ grid-row idx)]
            (p/server
              (GrayInput. true spec nil arg))
            (CellPad. grid-row 2))))
            )))

(p/defn Form [{::hf/keys [keys values] :as ctx}]
  (p/client
    (dom/form {:role  "form"
               :style {:border-left-color (c/color hf/db-name)}}
      (p/server
        (let [heights (vec (reductions + 0 (map height values)))]
          (into [] cat
            (p/for-by (comp first second) [[idx [key ctx]] (map-indexed vector (partition 2 (interleave keys values)))]
              (let [leaf? (= ::hf/leaf (::hf/type ctx))
                    argc  (count (::hf/arguments ctx))
                    h     (get heights idx)]
                (p/client
                  (let [row     (+ grid-row idx (- h idx))
                        dom-for (random-uuid)]
                    (p/for [i (range (dec grid-col))]
                      (cell row (inc i)))
                    (dom/label
                      {::dom/role  "cell"
                       ::dom/class "label"
                       ::dom/for   dom-for
                       ::dom/style {:grid-row         row
                                    :grid-column      grid-col
                                    #_#_:padding-left (str indentation "rem")}
                       ::dom/title (pr-str (or (spec-description false (attr-spec key))
                                             (p/server (schema-value-type hf/*schema* hf/db key))))}
                      (dom/text (str (non-breaking-padder indentation) (field-name key))))
                    (CellPad. row 2)
                    (binding [indentation (if true #_leaf? indentation (inc indentation))]
                      (binding [grid-row row
                                grid-col (inc grid-col)]
                        (p/server (GrayInputs. ctx)))
                      (binding [grid-row (if leaf? row (+ row argc))
                                grid-col (inc grid-col)]
                        (p/server
                          (let [ctx (assoc ctx ::dom/for dom-for)]
                            (if (::hf/options ctx)
                              (Default. ctx)
                              (Render. (assoc ctx ::dom/for dom-for))))))
                      )))))))))))

(p/defn Row [{::hf/keys [keys values] :as ctx}]
  (p/client
    (dom/tr
      (when-let [id (::group-id table-picker-options)]
        (let [value (p/server (hfql/JoinAllTheTree. ctx))]
          (dom/input
            {::dom/role    "cell"
             ::dom/type    :radio,
             ::dom/name    id,
             ::dom/checked (= (::current-value table-picker-options) value)
             ::dom/style   {:grid-row grid-row, :grid-column grid-col}})))
      (let [result (p/server
                     (into [] cat
                      (p/for-by second [[idx ctx] (map-indexed vector values)]
                        (p/client
                          (binding [grid-col (+ grid-col idx)]
                            (dom/td (p/server (binding [Form Default]
                                                (Render. ctx)))))))))]
        (p/for [i (range (dec grid-col))]
          (cell grid-row (inc i)))
        (CellPad. grid-row (inc (count keys)))
        result))))

(p/def Table)
(p/defn Table-impl [{::hf/keys [keys height] :as ctx} value]
  (let [actual-height (count value)]
    (p/client
      (PaginatedGrid (count keys) height actual-height
        (dom/table {::dom/role "table"}
          (dom/thead
            (dom/tr
              (p/for [i (range (dec grid-col))]
                (cell grid-row (inc i)))
              (when (::group-id table-picker-options)
                (dom/th {::dom/role  "cell"
                         ::dom/style {:grid-row grid-row, :grid-column grid-col}}))
              (p/for-by second [[idx col] (map-indexed vector keys)]
                (dom/th {::dom/role  "cell"
                         ::dom/class "label"
                         ::dom/title (pr-str (or (spec-description true (attr-spec col))
                                               (p/server (schema-value-type hf/*schema* hf/db col)))),
                         ::dom/style {:grid-row    grid-row,
                                      :grid-column (+ grid-col idx)
                                      :color       (c/color hf/db-name)}}
                  (str (non-breaking-padder indentation) (field-name col))))
              (CellPad. grid-row (inc (count keys)))))
          (dom/tbody
            (let [offset pagination-offset]
              (p/server
                (into [] cat
                  (let [vals     (->> value (drop offset) (take (dec height)))
                        vals-cnt (count vals)
                        res (p/for-by (comp ::key second) [[idx ctx] (map-indexed vector vals)]
                              (p/client (binding [grid-row (+ grid-row idx 1)]
                                          (p/server (Row. ctx)))))]
                    (p/client
                      (p/for [idx (range (max 0 (- height vals-cnt)))]
                        (binding [grid-row (+ grid-row vals-cnt idx)]
                          (CellPad. grid-row 0))))
                    res))))))))))


(defmacro PaginatedGrid [actual-width max-height actual-height & body]
  `(let [row-height#    (dom/measure "var(--hf-grid-row-height)")
         actual-height# (* row-height# (inc ~actual-height))
         !scroller#     (atom nil)
         !scroll-top#   (atom 0)]
     (dom/div {::dom/role  "scrollbar"
               ::dom/style {:grid-row-start grid-row
                            :grid-row-end   (+ grid-row ~max-height)
                            :grid-column    (+ grid-col ~actual-width)}}
       (do (reset! !scroller# dom/node)
           (let [[scroll-top#] (new (sw/scroll-state< dom/node))]
             (reset! !scroll-top# scroll-top#))
           nil)
       (dom/div {:role "filler" "data-height" actual-height# :style {:height (str actual-height# "px")}}))

     (binding [pagination-offset (max 0 (js/Math.ceil (/ (js/Math.floor (p/watch !scroll-top#)) row-height#)))]
       (dom/div {::dom/role "scrollview"}
        (dom/event "wheel" ; TODO support keyboard nav and touchscreens
          (fn [e#] (let [scroller# @!scroller#]
                     (set! (.. scroller# -scrollTop) (+ (.. scroller# -scrollTop) (.. e# -deltaY))))))
        ~@body))))

