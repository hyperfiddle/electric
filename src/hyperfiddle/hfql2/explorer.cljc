(ns hyperfiddle.hfql2.explorer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.explorer :as ex]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.hfql2.ui :as ui]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.api :as hf]
            [hyperfiddle.gridsheet :as gridsheet :refer [GridSheet]])
  #?(:cljs (:require-macros [hyperfiddle.hfql2.explorer])))

(p/defn Sequence ":: t m a -> m t a" [Vs] (p/fn [] (p/for [V Vs] (V.))))

(defmacro capture [dynvars & body]
  (let [syms (repeatedly #(gensym))]
    `(let [~@(interleave syms dynvars)]
       (p/fn []
         (binding [~@(interleave dynvars syms)]
           ~@body)))))

(p/def Rec)

(p/defn TreeToRows "Join all the tree, calling renderers when provided, return EDN" [V]
  (binding [Rec (p/fn [V]
                  (let [{::hf/keys [render cardinality columns leaf?]} (meta V)]
                    (if render
                      [(capture [Rec] [(render. V)])]
                      (let [v (V.)]
                        (case cardinality
                          ::hf/many (into [] cat (p/for [V v] (Rec. V)))
                          ;; infer
                          (cond
                            leaf?       [(p/fn [] [v])]
                            (vector? v) (into [] cat (p/for [V v] (Rec. V)))
                            (map? v)   (into [] cat (p/for [col columns]
                                                      (let [V (get v col)]
                                                        (if (::hf/leaf? (meta V))
                                                          [(capture [Rec] (into [col] cat (new (Sequence. (Rec. V)))))]
                                                          (into [(p/fn [] [col])]
                                                            (Rec. (get v col)))))))
                            :else       v))))))]
    (new Rec V)))

(defn inject-rows [rows]
  (assert (vector? rows) (every? vector? rows))
  (with-meta rows {:tag ::rows}))

(defn rows? [x] (= ::rows (:tag (meta x))))

(p/defn Identity [x] x)

(p/defn Options [{::hf/keys [summarize options continuation Value] :as ctx}]
  (let [value  (hf/JoinAllTheTree. ctx)
        labelf (or summarize Identity)]
    (p/client
      (dom/select
        (p/server
          (p/for [opt-e (options.)]
            (let [opt-value (hf/JoinAllTheTree. (continuation. opt-e))
                  selected? (= value opt-value)
                  text      (labelf. opt-value)]
              (p/client
                (dom/option {::dom/selected selected?} (dom/text text)))))))
      nil)))

(p/defn Header [{::hf/keys [attribute as] :as ctx}]
  (p/client (dom/span {::dom/class "hf-header"}
              (dom/text (cond (some? as)                   (name as)
                              (seq? attribute)             (name (first attribute))
                              (qualified-ident? attribute) (name attribute)
                              :else                        attribute)))
    nil))

(p/defn FormsTransposedToRows [{::hf/keys [keys] :as ctx} v depth]
  (inject-rows (into [(p/fn [] [depth (p/fn [] (p/for [k keys]
                                                 (p/fn [] (p/client (dom/span {::dom/class "hf-header"}
                                                                      (dom/text (if (seq? k) (name (first k)) (name k)))))
                                                   nil)))])]
                 cat (p/for [ctx v]
                       [(p/fn [] [depth (p/fn []
                                          (let [{::hf/keys [keys values]} ctx]
                                            (p/for-by first [[k ctx] (mapv vector keys values)]
                                              (p/fn []
                                                (let [{::hf/keys [render summarize options]} ctx]
                                                  (cond
                                                    options (Options. ctx)
                                                    render  (render. ctx)
                                                    :else   (let [value (hf/JoinAllTheTree. ctx)]
                                                              (if summarize
                                                                (summarize. value)
                                                                (ui/SpecDispatch. ctx)))))))) )])]))))

#_(p/defn ListOfFormsWithIdentityHeader [V]
  (let [v     (V.)
        depth (::depth (meta V))]
    (inject-rows (into [(p/fn [] [depth (p/fn [] (p/for [col (::hf/columns (meta V))]
                                                   (p/fn [] col)))])]
                   cat (p/for [V v]
                         (into [(p/fn [] [depth (p/fn [] [ (p/fn [] (new (:db/id (V.))))])])]
                           (Rec. V (inc depth))))))))

(p/defn FormLabel [{::hf/keys [attribute arguments options] :as ctx} depth]
  (into [(p/fn [] [depth (p/fn [] [(p/fn [] (Header. ctx))
                                   (p/fn [] (when options (Options. ctx)))])])]
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
    (FormsTransposedToRows. ctx v (inc depth))))

(defmacro rows [& rows] `[(p/fn [] ~@rows)])
(defmacro row [depth cols] `[~depth (p/fn [] ~cols)])
(defmacro col [v] `(p/fn [] ~v))

(p/defn TreeToExplorer [ctx]
  (binding [Rec (p/fn [{::hf/keys [type render keys values Value] :as ctx} depth]
                  (if render
                    (let [v (render. (assoc ctx ::depth depth))]
                      (if (rows? v)
                        v
                        [(capture [Rec] [(render. ctx)])]))
                    (case type
                      ::hf/leaf (rows (row depth [(col (Value.))]))
                      ::hf/keys (into [] cat (p/for-by first [[k ctx] (mapv vector keys values)]
                                               (if (= ::hf/leaf (::hf/type ctx))
                                                 [(capture [Rec] (row depth [(col (Header. ctx))
                                                                             (col (if-let [render (::hf/render ctx)]
                                                                                    (render. ctx)
                                                                                    (ui/SpecDispatch. ctx)))]))]
                                                 (into (FormLabel. ctx depth)
                                                   (Rec. ctx (inc depth))))))
                      (let [v (Value.)]
                        (cond (vector? v) (HandleCardMany. ctx v depth) ; card many
                              (map? v)    (Rec. v depth)                ; card one
                              :else       (throw (ex-info "unreachable" {:value v})))))))]
    (new Rec ctx 0)))

(p/defn EdnRender "Join all the tree, calling renderers when provided, return EDN" [V]
  (binding [Rec (p/fn [{::keys [type render keys Value values] :as ctx}]
                  (if render (render. ctx)
                      (case type
                        ::leaf (Value.)
                        ::keys (into {} (zipmap keys (p/for [ctx values] (Rec. ctx))))
                        (let [ctx (Value.)]
                          (cond
                            (vector? ctx) (p/for [ctx ctx] (Rec. ctx))
                            (map? ctx)    (Rec. ctx)
                            :else         ctx)))))]
    (new Rec V)))

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
