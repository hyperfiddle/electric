(ns hyperfiddle.hfql.explorer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.explorer :as ex]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.hfql.ui :as ui]
            [hyperfiddle.hfql :as hfql]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.api :as hf]
            [hyperfiddle.gridsheet :as gridsheet :refer [GridSheet]])
  #?(:cljs (:require-macros [hyperfiddle.hfql.explorer])))

(p/defn Sequence ":: t m a -> m t a" [Vs] (p/fn [] (p/for [V Vs] (V.))))

(defmacro capture [dynvars & body]
  (let [syms (repeatedly #(gensym))]
    `(let [~@(interleave syms dynvars)]
       (p/fn []
         (binding [~@(interleave dynvars syms)]
           ~@body)))))

(p/def Rec)

(defn inject-rows [rows]
  (assert (vector? rows) (every? vector? rows))
  (with-meta rows {:tag ::rows}))

(defn rows? [x] (= ::rows (:tag (meta x))))

(p/defn Identity [x] x)

(p/defn OptionsRenderer [ctx] nil)

(p/defn SelectOptions [{::hf/keys [summarize options continuation Value] :as ctx}]
  (let [value  (hfql/JoinAllTheTree. ctx)
        labelf (or summarize Identity)]
    (p/client
      (dom/select
        (p/server
          (p/for [opt-e (options.)]
            (let [opt-value (hfql/JoinAllTheTree. (continuation. opt-e))
                  selected? (= value opt-value)
                  text      (labelf. opt-value)]
              (p/client
                (dom/option {::dom/selected selected?} (dom/text text)))))))
      nil)))

(defn key-name [k] (cond (seq? k)             (name (first k))
                         (qualified-ident? k) (name k)
                         :else                k))

(p/defn HeaderRenderer [{::hf/keys [as attribute]} k] (or k as attribute))

(p/defn TableHeader [{::hf/keys [attribute as] :as ctx} k]
  (let [attribute (key-name (or k as attribute))]
    (p/client (dom/span {::dom/class "hf-header"}
                (dom/text attribute))
      nil)))

(p/defn CellRenderer [ctx value]
  (if (= ::nil value)
    (new (::hf/Value ctx))
    value))

(p/defn FormsTransposedToRows
  [{::hf/keys [keys]
    ::keys    [RenderHeader RenderCell RenderOptions]
    :as       ctx}
   v
   depth]
  (inject-rows
    (into [(p/fn [] [depth (p/fn [] (p/for [k keys] (p/fn [] (RenderHeader. ctx k))))])]
      cat (p/for [ctx v]
            [(p/fn [] [depth (p/fn []
                               (let [{::hf/keys [keys values]} ctx]
                                 (p/for-by first [[k ctx] (mapv vector keys values)]
                                   (p/fn []
                                     (let [{::hf/keys [render summarize options]} ctx]
                                       (cond render  (render. ctx)
                                             options (RenderOptions. ctx)
                                             :else   (let [value (hfql/JoinAllTheTree. ctx)]
                                                       (if summarize
                                                         (summarize. value)
                                                         (RenderCell. ctx value)))))))) )])]))))

(p/defn FormLabelRenderer [{::hf/keys [attribute] :as ctx} depth]
  [(p/fn [] [depth (p/fn [] [(p/fn [] attribute)])])])

(p/defn FormLabelRendererWithInputs
  [{::hf/keys [attribute arguments options]
    ::keys    [RenderOptions RenderHeader]
    :as       ctx}
   depth]
  (into [(p/fn [] [depth (p/fn [] [(p/fn [] (RenderHeader. ctx nil))
                                   (p/fn [] (when options (RenderOptions. ctx)))])])]
    cat
    (p/for-by first [[arg-name props :as arg] arguments]
      [(p/fn [] [(inc depth) (p/fn []
                               (let [spec (ui/attr-spec attribute)]
                                 [(p/fn [] (p/client (dom/span {::dom/title (pr-str (:hyperfiddle.spec/form (spec/arg spec arg-name)))}
                                                       (dom/text (str "🔎 " (name arg-name)))) nil))
                                  (p/fn [] (ui/GrayInput. false spec {::dom/placeholder "bob…"} arg) nil)]))])])))

(p/defn HandleCardMany [{::hf/keys [type] :as ctx} v depth]
  (case type
    ::hf/leaf (into [] cat (p/for [ctx v] (Rec. ctx (inc depth))))
    (FormsTransposedToRows. ctx v depth)))

(defmacro rows [& rows] `[(p/fn [] ~@rows)])
(defmacro row [depth cols] `[~depth (p/fn [] ~cols)])

(defn convey [ctx-a ctx-b] (merge ctx-b (select-keys ctx-a [::RenderCell ::RenderHeader ::RenderFormLabel ::RenderOptions])))

(p/defn TreeToExplorer [{::keys [RenderFormLabel RenderCell RenderHeader RenderOptions]
                         :or    {RenderCell      CellRenderer
                                 RenderHeader    HeaderRenderer
                                 RenderFormLabel FormLabelRenderer
                                 RenderOptions   OptionsRenderer}
                         :as    root-ctx}]
  (binding [Rec (p/fn [{::hf/keys [type render keys values Value] :as ctx} depth]
                  (let [ctx (merge ctx {::RenderCell      RenderCell
                                        ::RenderHeader    RenderHeader
                                        ::RenderFormLabel RenderFormLabel
                                        ::RenderOptions   RenderOptions})]
                    (if render
                      (let [v (render. (assoc ctx ::depth depth))]
                        (if (rows? v)
                          v
                          [(capture [Rec] [depth (p/fn [] [(p/fn [] v)])])]))
                      (case type
                        ::hf/leaf (rows (row depth [(p/fn [] (Value.))]))
                        ::hf/keys (into [] cat (p/for-by first [[k ctx'] (mapv vector keys values)]
                                                 (let [ctx (convey ctx ctx')]
                                                   (if (= ::hf/leaf (::hf/type ctx))
                                                     [(capture [Rec] (row depth [(p/fn [] (RenderHeader. ctx nil))
                                                                                 (p/fn [] (if-let [render (::hf/render ctx)]
                                                                                            (render. ctx)
                                                                                            (RenderCell. ctx ::nil)))]))]
                                                     (into (RenderFormLabel. ctx depth)
                                                       (Rec. ctx (inc depth)))))))
                        (let [v (Value.)]
                          (cond (vector? v) (HandleCardMany. ctx v depth) ; card many
                                (map? v)    (Rec. v depth)                ; card one
                                :else       (throw (ex-info "unreachable" {:value v}))))))))]
    (new Rec root-ctx 0)))

(defn col->idx "Return the numeric index of a Excel-like column name (a string).
  e.g.: (col->idx \"AA\") := 26"
  [col]
  (assert (and (string? col) (not-empty col)))
  (dec ;; 0 based
    (reduce (fn [r char] (+ (* r 26) (inc (- #?(:clj (int char), :cljs (.charCodeAt char 0)) 65)) ))
      0
      (clojure.string/upper-case col))))

(defn idx->col [idx]
  (assert (>= idx 0))
  (let [n    (mod idx 26)
        rest (int (/ idx 26))
        char (char (+ 65 n))]
    (if (pos? rest)
      (str (idx->col (dec rest)) char)
      (str char))))

(defn column-range
  ([end] (column-range 0 end))
  ([start end]
   (mapv idx->col (range start end))))

(defn parse-props [{::keys [page-size row-height columns]
                    :or    {page-size  40
                            row-height 24
                            columns    2}
                    :as    props}]
  (merge
    {:hyperfiddle.explorer/page-size   page-size
     :hyperfiddle.explorer/row-height  row-height
     ::dom/style                       {:height (str "calc(("page-size" + 1) * "row-height"px)")}
     ::gridsheet/grid-template-columns (str "20rem repeat(" (dec columns)", 1fr)")}
    props))

(p/defn Explorer [props hfql]
  (let [xs (new (Sequence. (TreeToExplorer. hfql)))]
    (binding [ex/cols   (if-let [columns (::columns props)]
                          (column-range columns)
                          ex/cols)
              ex/Format (p/fn [M a]
                          (let [row (M.)]
                            (some-> (get row (col->idx a))
                              (new)
                              (pr-str))))]
      (ex/BasicExplorer. (parse-props props) xs))))

(p/defn ExplorerWithUI [props hfql]
  (Explorer. props (merge {::RenderCell      (p/fn [ctx _] (ui/SpecDispatch. ctx))
                           ::RenderHeader    TableHeader
                           ::RenderFormLabel FormLabelRendererWithInputs
                           ::RenderOptions   SelectOptions}
                     hfql)))
