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

;; (p/defn SelectMultiple [V]
;;   (let [{::hf/keys [summarize options continuation]} (meta V)
;;         values (hf/JoinAllTheTree. V)
;;         selected? (set values)
;;         labelf (or summarize Identity)]
;;     (p/client
;;       (dom/select {::dom/multiple true, ::dom/size 1}
;;         (p/server
;;           (p/for [v values]
;;             (let [text (labelf. v)
;;                   selected? (selected? v)]
;;               (p/client
;;                 (dom/option {::dom/selected selected?} (dom/text text)))))))
;;       nil)))

(p/defn Options [V]
  (let [{::hf/keys [summarize options continuation]} (meta V)
        value (hf/JoinAllTheTree. V)
        labelf (or summarize Identity)]
    (p/client
      (dom/select
        (p/server
          (p/for [opt-e (options.)]
            (let [opt-value (hf/JoinAllTheTree. (p/partial 1 continuation opt-e))
                  selected? (= value opt-value)
                  text (labelf. opt-value)]
              (p/client
                (dom/option {::dom/selected selected?} (dom/text text)))))))
      nil)))

(p/defn Header [col props]
  (let [as (::hf/as props)]
    (p/client (dom/span {::dom/class "hf-header"} (dom/text (cond (some? as)             (name as)
                                                                  (seq? col)             (name (first col))
                                                                  (qualified-ident? col) (name col)
                                                                  :else                  col)))
      nil)))

(p/defn FormsTransposedToRows [V]
  (let [v     (V.)
        depth (::depth (meta V))]
    (inject-rows (into [(p/fn [] [depth (p/fn [] (p/for [col (::hf/columns (meta V))]
                                                   (p/fn [] (Header. col (meta V)))))])]
                   cat (p/for [V v]
                         [(p/fn [] [depth (p/fn []
                                            (let [v (V.)]
                                              (p/for [col (::hf/columns (meta V))]
                                                (p/fn []
                                                  (let [V                                            (get v col)
                                                        {::hf/keys [render summarize options] :as m} (meta V)]
                                                    (cond
                                                      options (Options. V)
                                                      render  (render. V)
                                                      :else   (let [value (hf/JoinAllTheTree. V)]
                                                                (if summarize
                                                                  (summarize. value)
                                                                  (ui/SpecDispatch. value m)))))))) )])])))))

#_(p/defn ListOfFormsWithIdentityHeader [V]
  (let [v     (V.)
        depth (::depth (meta V))]
    (inject-rows (into [(p/fn [] [depth (p/fn [] (p/for [col (::hf/columns (meta V))]
                                                   (p/fn [] col)))])]
                   cat (p/for [V v]
                         (into [(p/fn [] [depth (p/fn [] [ (p/fn [] (new (:db/id (V.))))])])]
                           (Rec. V (inc depth))))))))

(p/defn FormLabel [V depth]
  (let [{::hf/keys [attribute arguments] :as m} (meta V)]
    (into [(p/fn [] [depth (p/fn [] [(p/fn [] (Header. attribute m ))
                                     (p/fn [] (let [{::hf/keys [options]} (meta V)]
                                               (when options (Options. V))))])])]
      cat
      (p/for-by first [[arg-name props :as arg] arguments]
        [(p/fn [] [(inc depth) (p/fn []
                                 (let [spec (ui/attr-spec attribute)]
                                   [(p/fn [] (p/client (dom/span {::dom/title (pr-str (:hyperfiddle.spec/form (spec/arg spec arg-name)))}
                                                         (dom/text (str "🔎 " (name arg-name)))) nil))
                                    (p/fn [] (ui/GrayInput. false spec {::dom/placeholder "bob…"} arg) nil)]))])]))))

(p/defn TreeToExplorer [V]
  (binding [Rec (p/fn [V depth]
                  (let [{::hf/keys [render cardinality columns leaf?]} (meta V)]
                    (if render
                      (let [v (render. (vary-meta V assoc ::depth depth))]
                        (if (rows? v)
                          v
                          [(capture [Rec] [(render. V)])]))
                      (let [v (V.)]
                        (case cardinality
                          ::hf/many (into [] cat (p/for [V v] (Rec. V (inc depth))))
                          ;; infer
                          (cond
                            leaf?       [(p/fn [] [depth (p/fn [] [(p/fn [] v)])])]
                            (vector? v) (into [] cat (p/for [V v] (Rec. V (inc depth))))
                            (map? v)    (into [] cat (p/for [col columns]
                                                       (let [V (get v col)]
                                                         (if (::hf/leaf? (meta V))
                                                           [(capture [Rec] [depth
                                                                            (p/fn []
                                                                              (into [(p/fn [] (Header. col (meta V)))] cat
                                                                                [[(p/fn [] (if-let [render (::hf/render (meta V))]
                                                                                             (render. V)
                                                                                             (ui/SpecDispatch. (V.) (meta V))))]]))])]
                                                           (into (FormLabel. V depth)
                                                             (Rec. (get v col) (inc depth)))))))
                            :else       (throw (ex-info "unreachable" {}))))))))]
    (new Rec V 0)))

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

(p/defn Explorer [props hfql]
  (let [xs (new (Sequence. (TreeToExplorer. hfql)))]
    (binding [ex/Format (p/fn [M a]
                          (let [row (M.)]
                            (some-> (get row (col->idx a))
                              (new)
                              (pr-str))))]
      (ex/BasicExplorer. props xs))))
