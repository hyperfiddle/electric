(ns hyperfiddle.q4
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [cljs.analyzer.api :as ana-api]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests ! %]]
            [user.gender-shirt-size :refer [submissions submission shirt-sizes genders]])
  #?(:cljs (:require-macros [hyperfiddle.q4 :refer [hfql id-as-string-renderer select-option-renderer ignoring-renderer throwing-renderer]])))

(defn replace* [smap coll]
  (if (seqable? coll)
    (replace smap coll)
    coll))

(defn compile-leaf* [?form]
  (cond
    (keyword? ?form) `(unquote (hf/nav ~'entity ~?form))
    (seq? ?form)     ?form))

(p/def value-mode false)

(defn render [val props]
  `#'(p/binding ~[`hf/value val, `hf/props (not-empty (dissoc props ::hf/render ::hf/as))]
       (if value-mode
         (unquote hf/render)
         (unquote ~(if-let [renderer (::hf/render props)]
                     renderer
                     `hf/render)))))

(defn has-props? [form]
  (and (sequential? form)
       (or (keyword? (first form))
           (sequential? (first form)))))

(def hfql-prop? #{::hf/options ::hf/a})

(defn extract-props [form]
  (if (has-props? form)
    (let [[form & {:keys [] :as props}] form]
      (if (sequential? form)
        (if (and (keyword? (first form))
                 (or (= 1 (count form))
                     (= 2 (count form))))
          [(first form) props]
          [form props])
        [form props]))
    [form nil]))

(defn resolve' [env x]
  (if (symbol? x)
    (doto (if (:js-globals env)
            (:name (ana-api/resolve env x))
            (get env x))
      prn)
    x))

(defn qualify [env& ns-map form]
  (walk/prewalk (fn [x]
                  (if (and (simple-symbol? x))
                    (or (resolve' env& x)
                        (if-let [?var (get ns-map x)]
                          (if (var? ?var) ; lexically bound if not a var
                            (symbol ?var)
                            x)
                          x))
                    x))
                form))

(defn qualify-link [env& ns-map form]
  (let [[f & args] form]
    (cons `list (qualify env& ns-map (cons (list 'quote f) args)))))

(declare compile-hfql*)

(defn call [[f & args]]
  (if (= `unquote f)
    (cons f args)
    (cons `p/$ (cons f args))))

(p/defn render-input [f arg-index] "")

(defn inputs-indices [form]
  (when (seqable? form)
    (->> form (map-indexed vector) (filter (comp #{'.} second)) (map first))))

(defn gen-inputs [form]
  (when-some [indices (seq (inputs-indices form))]
    (let [bindings (map (fn [index]
                          (let [id (gensym "list")]
                            [id `(gensym "list"), (gensym (str "input-" index "-")) `(p/$ render-input ~id '~(first form) ~index)])) indices)
          list-ids (zipmap indices (map first bindings))
          names    (zipmap indices (map #(nth % 2) bindings))]
      [list-ids names bindings])))

;; (gen-inputs '(shirt-sizes foo .))

(defn replace-inputs [indices form] (map-indexed (fn [idx x] (get indices idx x)) form))

(defn rewrite-option-inputs [form]
  (let [list-id (str (gensym))]
    (if-let [inputs (gen-inputs form)]
      (let [[list-ids names bindings] inputs]
        `(let [~@(mapcat identity bindings)]
           [~list-ids ~(call (replace-inputs names form))]))
      `[nil ~(call form)])))

;; (rewrite-option-inputs '(shirt-sizes foo . .))

(defn expand-props [env& ns-map env' props]
  (reduce-kv (fn [r k v]
               (if (hfql-prop? k)
                 (case k
                   ::hf/a       (assoc r k (qualify-link env& ns-map v))
                   ::hf/options (assoc r k (list `var (rewrite-option-inputs v)))
                   (assoc r k (second (val (first (compile-hfql* env& ns-map env' v))))))
                 r))
             props props))

(defn quote* [env& env' xs]
  (walk/postwalk (fn [x]
                   (cond
                     (list? x)         (list 'quote x)
                     (get env' x)      (get env' x)
                     (resolve' env& x) x
                     (keyword? x)      x
                     :else             x))
                 xs))

;; TODO look at spec
(def cardinality* {'user.gender-shirt-size/submissions :db.cardinality/many
                   'user.gender-shirt-size/shirt-sizes :db.cardinality/many
                   'user.gender-shirt-size/genders     :db.cardinality/many
                   'user.gender-shirt-size/submission  :db.cardinality/one})

(defn cardinality [form]
  (cond
    (symbol? form)  (get cardinality* form)
    (keyword? form) (get cardinality* form)
    (seq? form)     (let [[f & args] form]
                      (get cardinality* f)
                      #_(:db/cardinality (meta f)))))

(tests
  (cardinality '(user.gender-shirt-size/shirt-sizes _ nil)) := :db.cardinality/many
  (cardinality 'user.gender-shirt-size/shirt-sizes) := :db.cardinality/many)

(defn many? [form] (= :db.cardinality/many (cardinality form)))

(tests
  (many? :dustingetz/gender) := false
  (many? 'user.gender-shirt-size/shirt-sizes) := true
  (many? 'user.gender-shirt-size/shirt-size) := false
  (many? '(user.gender-shirt-size/shirt-sizes a nil)) := true
  (many? '(user.gender-shirt-size/shirt-size a)) := false)

(defn drop-slash [kw-sym]
  (symbol (str (namespace kw-sym) "__" (name kw-sym))))

(defn hf-edge->sym! [env edge]
  (if-let [sym (get env edge)]
    [env sym]
    (let [[_ {:keys [::hf/as]}] (extract-props edge)]
      (if (some? as)
        (cond
          (keyword? edge) (if (namespace edge)
                            (let [sym (drop-slash edge)]
                              [(assoc env as sym) sym])
                            [env (symbol (name edge))])
          :else           [env nil])
        [env edge]))))

(defn aliased-forms [forms]
  (->> forms
       (map (fn [form] (let [[form props] (extract-props form)]
                         [form (::hf/as props)])))
       (filter second)
       (into {})))

(defn put-aliases [env aliases-map]
  (into (or env {}) (map (fn [[k v]] [[:alias k] v]) aliases-map)))

(defn get-alias [env k]
  (get env [:alias k]))

(defn aliased-forms [tree]
  (->> (tree-seq coll? seq tree)
       (filter seq?)
       (filter #(some #{::hf/as} %))))

(tests
  (aliased-forms '[(:dustingetz/shirt-size ::hf/options (shirt-sizes gender nil))
                   {:dustingetz/gender [(:db/ident ::hf/as gender)]}])
  := '((:db/ident ::hf/as gender)))

(defn aliased-form-path* [tree form]
  (cond (= form tree)  []
        (map? tree)    (some (fn [[k v]]
                               (when-let [p (aliased-form-path* v form)]
                                 (if (seq? k)
                                   (cons (first k) p)
                                   (cons k p))))
                             tree)
        (vector? tree) (some identity (map-indexed (fn [idx v]
                                                     (when-let [p (aliased-form-path* v form)]
                                                       (cons idx p)))
                                                   tree))))

(defn aliased-form-path [tree form]
  (let [path (aliased-form-path* tree form)]
    (vec (concat (remove number? path) [(first form)]))))

(tests
  (aliased-form-path* '[(:dustingetz/shirt-size ::hf/options (shirt-sizes gender nil))
                        {(:dustingetz/gender) [(:db/ident ::hf/as gender)]}]
                      '(:db/ident ::hf/as gender))
  := '(1 :dustingetz/gender 0))

(tests
  (aliased-form-path '[(:dustingetz/shirt-size ::hf/options (shirt-sizes gender nil))
                       {:dustingetz/gender [(:db/ident ::hf/as gender)]}]
                     '(:db/ident ::hf/as gender))
  := '(:dustingetz/gender :db/ident))

(defn aliases [aliased-forms]
  (into {} (map (fn [[_ & {:keys [::hf/as]} :as form]] [as form]) aliased-forms)))

(tests
  (aliases '((:db/ident ::hf/as ident)))
  := '{ident (:db/ident ::hf/as ident)})

(tests
  (aliases (aliased-forms '[(:dustingetz/shirt-size ::hf/options (shirt-sizes gender nil))
                            {:dustingetz/gender [(:db/ident ::hf/as gender)]}]))
  := '{gender (:db/ident ::hf/as gender)})

(defn has-alias? [[_f & {:keys [::hf/as]}]] as)

(defn closest-alias-common-ancestor [tree]
  (let [syms    (->> (if (map? tree) ;; TODO rename extract-next-level
                       (keys tree)
                       ;; vector
                       (filter identity (mapcat (fn [form]
                                                  (cond (map? form) (keys form)
                                                        (seq? form) (if-not (has-alias? form) (list form) nil)
                                                        :else       nil))
                                                tree)))
                     (tree-seq coll? seq)
                     (filter symbol?)
                     (set))
        aliases (set (keys (aliases (aliased-forms tree))))]
    (set/intersection syms aliases)))

(tests
  (closest-alias-common-ancestor '[{(:dustingetz/shirt-size ::hf/options (shirt-sizes gender nil)) [*]}
                                   {:dustingetz/gender [(:db/ident ::hf/as gender)]}])
  := '#{gender}

  (closest-alias-common-ancestor '[(:db/ident ::hf/as gender)])
  := #{})

(defn form-contains? [form expr]
  (boolean (some (if (set? expr) expr #{expr}) (tree-seq coll? seq form))))

(tests
  (form-contains? '[{:dustingetz/gender [(:db/ident ::hf/as gender)]}]
                  '(:db/ident ::hf/as gender))
  := true)

(defn aliases-here [tree]
  (let [common  (closest-alias-common-ancestor tree)
        aliases (aliases (aliased-forms tree))]
    (select-keys aliases common)))

(tests
  (aliases-here '[{(:dustingetz/shirt-size ::hf/options (shirt-sizes gender nil)) [*]}
                  {:dustingetz/gender [(:db/ident ::hf/as gender)]}])
  := '{gender (:db/ident ::hf/as gender)}

  (aliases-here '[(:db/ident ::hf/as gender)]) := {})

;; (defn lazy-accessor
;;   ([sym path] (list `var (second (lazy-accessor sym path `(unquote ~sym)))))
;;   ([sym path r]
;;    (if (empty? path)
;;      r
;;      (recur sym (rest path) `(unquote (get ~r ~(first path)))))))

(defn lazy-accessor [sym path]
  (list `var (apply list '-> sym `(unquote) (concat (interpose `(unquote) (map #(list 'get %) path)) `((unquote))))))

(tests
  (lazy-accessor 'gender [:dustingetz/gender :db/ident])
  := '#'(-> gender
            (clojure.core/unquote)
            (get :dustingetz/gender)
            (clojure.core/unquote)
            (get :db/ident)
            (clojure.core/unquote)))

(defn aliases-bindings [tree aliases-map]
  (->> (reduce-kv (fn [r alias form]
                    (if-some [form (first (filter #(form-contains? % form) tree))]
                      (assoc r alias form)
                      r))
                  {}
                  aliases-map)
       (reduce-kv (fn [r alias form]
                    (let [alias-form-name (symbol (str alias "_form"))
                          aliased         (get (aliases (aliased-forms form)) alias)
                          path            (aliased-form-path form aliased)]
                      (-> (assoc-in r [:accessors alias] (lazy-accessor alias-form-name path))
                          (assoc-in [:forms alias-form-name] form)
                          (assoc-in [:names (first path)] alias-form-name)
                          (assoc-in [:refs alias] `(p/binding [value-mode true] (unquote ~alias))))))
                  {})))

(tests
  (aliases-bindings '[{(:dustingetz/shirt-size ::hf/options (shirt-sizes gender nil)) [*]}
                      {:dustingetz/gender [(:db/ident ::hf/as gender)]}]
                    '{gender (:db/ident ::hf/as gender)})
  := '{:accessors {gender #'(-> gender_form (clojure.core/unquote) (get :dustingetz/gender) (clojure.core/unquote) (get :db/ident) (clojure.core/unquote))},
       :forms     {gender_form #:dustingetz{:gender [(:db/ident :hyperfiddle.api/as gender)]}},
       :names     #:dustingetz{:gender gender_form},
       :refs      {gender (hfdl.lang/binding [hyperfiddle.q4/value-mode true] ~gender)}})

(defn topo-paths
  ([deps-map leaf] (topo-paths deps-map leaf (list leaf)))
  ([deps-map point acc]
   (if-some [deps (seq (get deps-map point))]
     (mapcat (fn [dep] (topo-paths deps-map dep (cons dep acc))) deps)
     (list acc))))

(tests
  (topo-paths {:d #{:b :c}, :c #{:b}, :b #{:a}, :a #{}} :d) := '((:a :b :c :d) (:a :b :d)))

(defn topo-rank [path] (into {} (map-indexed (fn [idx x] [x idx]) path)))

(defn topo-ranks [paths]
  (->> (map topo-rank paths)
       (apply merge-with max)))

(tests
  (topo-ranks '((:a :b :c :d) (:a :b :d))) := {:a 0, :b 1, :c 2, :d 3})

(defn flipmap [m]
  (reduce-kv (fn [r k v]
               (assoc r k (mapv key v)))
             {}
             (group-by val (seq m))))

(tests
  (flipmap {:a 1, :b 2}) := {1 [:a], 2 [:b]}
  (flipmap {:a 1, :b 1}) := {1 [:a :b]})

(defn toposort [kv-map] ; We want to order map entries by their relative symbolic deps
  (let [refs     (set (keys kv-map))
        ;; Build a map of {:c #{:a :b}}, where :b depends on :a and :b
        deps-map (reduce-kv (fn [r k v]
                              (assoc r k (set (filter refs (tree-seq coll? seq v)))))
                            {} kv-map)
        ;; Find all entries without dependents, here #{:c}
        leaves   (set/difference (set (keys deps-map)) (apply set/union (vals deps-map)))
        ;; compute position of each entry relative to others, e.g. {:a 1, :b 1, :c 2}
        ranks    (topo-ranks (mapcat (partial topo-paths deps-map) leaves))]
    ;; gather original forms in the right order
    (->> (flipmap ranks)
         (sort-by key)
         (mapcat (fn [[k vs]] (seq (select-keys kv-map vs)))))))

(tests
  (toposort '{a b, b c, c d}) := '([c d] [b c] [a b]))

(defmacro let-toposorted [bindings & body]
  `(let [~@(mapcat identity (toposort (into {} (map vec (partition 2 bindings)))))]
     ~@body))

(tests
  (let-toposorted [b a
                   c b
                   a 1]
    c) := 1)

(defn rewrite [subst-map form] (walk/postwalk (fn [x] (get subst-map x x)) form))

(defn compile-hfql*
  [env& ns-map env' form]
  (cond
    (vector? form)
    (if-some [aliases (not-empty (aliases-here form))]
      (let [{:keys [accessors forms names refs]} (aliases-bindings form aliases)
            forms-bindings                       (mapcat identity (reduce-kv (fn [r k v] (assoc r k (list `var (compile-hfql* env& ns-map env' v)))) {} forms))
            bindings                             (concat forms-bindings (mapcat identity accessors))
            env'                                 (put-aliases env' names)]
        `(let-toposorted [~@bindings]
           ~(apply merge (map (partial compile-hfql* env& ns-map env') (rewrite refs form)))))
      (apply merge (map (partial compile-hfql* env& ns-map env') form)))

    (map? form)
    (reduce-kv (fn [r edge cont]
                 (let [[edge props]                               (extract-props edge)
                       qedge                                      (qualify env& ns-map edge)
                       edge*                                      (compile-leaf* edge)
                       [env' edge-sym]                            (hf-edge->sym! env' edge)
                       props                                      (expand-props env& ns-map env' props)
                       [input-ids inputs-indices inputs-bindings] (gen-inputs edge)
                       body-content                               (compile-hfql* env& ns-map env' cont)
                       body                                       (if (many? qedge)
                                                                    (render (list `var body-content) nil)
                                                                    (if (seq inputs-bindings)
                                                                      (render (list `var `(let [~@(mapcat identity inputs-bindings)] [~input-ids ~body-content])) props)
                                                                      (render (list `var body-content) props)))
                       value                                      (let [call-form (call (replace* env' (replace-inputs inputs-indices edge*)))]
                                                                    (if-let [alias (get-alias env' edge)]
                                                                      `(get (unquote ~alias) ~edge)
                                                                      (if (many? qedge)
                                                                        (render `#'(let [~@(mapcat identity inputs-bindings)]
                                                                                     [~input-ids (p/for [~'entity ~call-form] ~body)]) props)
                                                                        `(let [~'entity ~call-form]  ~body))))]
                   (merge r `{~(quote* env& env' qedge) ~value})))
               {} form)

    :else (let [[form props] (extract-props form)
                props        (expand-props env& ns-map env' props)
                qedge        (qualify env& ns-map form)]
            {`~(quote* env& env' qedge) (if-let [alias (get-alias env' form)]
                                          `(get (unquote ~alias) ~form)
                                          (render (list `var (compile-leaf* (replace* env' form))) props))})))

(defmacro hfql [form]
  `(p/binding [hf/value ~(list `var (compile-hfql* &env (ns-map *ns*) {} (if-not (vector? form) [form] form)))]
     (unquote hf/render)))

(def exports (merge p/exports hf/exports))

;; #?(:clj (do
;;;;;;;;;;;
;; TESTS ;;
;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;
;; CARDINALITY 1 ;;
;;;;;;;;;;;;;;;;;;;

(tests
  (p/run (! (let [entity 9] (hfql :db/id))))
  % := {:db/id 9}
  )

(p/defn id-as-string-renderer [] (str ~hf/value))

(tests
  (p/run (! (let [entity 9] (hfql (:db/id ::hf/render id-as-string-renderer)) )))
  % := {:db/id "9"})

(tests
  (p/run (! (p/binding [value-mode 1] value-mode)))
  %)

(tests
  (p/run (! (hfql {(submission "") [:db/id]})))
  % := {'(user.gender-shirt-size/submission "") {:db/id 9}})

(tests
  (p/run (! (hfql {(submission "") [{:dustingetz/shirt-size [:db/ident]}]})))
  % := {'(user.gender-shirt-size/submission "") #:dustingetz{:shirt-size #:db{:ident :dustingetz/womens-large}}})

;;;;;;;;;;;;;;;;;;;
;; CARDINALITY N ;;
;;;;;;;;;;;;;;;;;;;

(tests
  (p/run (! (hfql {(submissions "") [:db/id]})))
  % := {'(user.gender-shirt-size/submissions "") [{:db/id 9} {:db/id 10} {:db/id 11}]})

(tests
  (p/run (! (hfql {(submissions "") [(:db/id ::hf/render id-as-string-renderer)]})))
  % := {'(user.gender-shirt-size/submissions "") [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})

;;;;;;;;;;;;;
;; OPTIONS ;;
;;;;;;;;;;;;;

(p/def select-option-renderer
  #'(into [:select]
          (p/for [e ~(::hf/options hf/props)]
            [:option ~(hf/nav e :db/ident)])))

(tests
  (p/run (! (let [entity 9]
              (hfql (:dustingetz/shirt-size ::hf/render select-option-renderer
                                            ::hf/options (shirt-sizes :dustingetz/female ""))))))
  %
  := #:dustingetz{:shirt-size [:select
                               [:option :dustingetz/womens-small]
                               [:option :dustingetz/womens-medium]
                               [:option :dustingetz/womens-large]]})

(defn fail []
  (throw (ex-info "I fail" {})))

(p/def throwing-renderer #'(fail))

(p/def ignoring-renderer #'"ignored")

(tests
  (p/run (! (hfql {((submission "")) [{(:dustingetz/gender ::hf/render ignoring-renderer) [(:db/ident ::hf/render throwing-renderer)]}]}) ))
  % := {'(user.gender-shirt-size/submission "") #:dustingetz{:gender "ignored"}} ; note it didn’t throw
  )

;;;;;;;;;
;; ENV ;;
;;;;;;;;;

(p/defn render-typeahead []
  [:select {:value ~hf/value}
   (p/for [e ~(::hf/options hf/props)]
     [:option ~(hf/nav e :db/ident)])])

(tests
  (p/run (! (hfql [{(submissions "")
                    [:db/id :dustingetz/email
                     (:dustingetz/shirt-size ::hf/render render-typeahead
                                             ::hf/options (shirt-sizes gender nil))
                     {:dustingetz/gender [(:db/ident ::hf/as gender)]}]}]) ))
  % := '{(user.gender-shirt-size/submissions "")
         [{:dustingetz/gender #:db{:ident :dustingetz/female},
           :dustingetz/email "alice@example.com",
           :dustingetz/shirt-size
           [:select
            {:value :dustingetz/womens-large}
            [[:option :dustingetz/womens-small]
             [:option :dustingetz/womens-medium]
             [:option :dustingetz/womens-large]]],
           :db/id 9}
          {:dustingetz/gender #:db{:ident :dustingetz/male},
           :dustingetz/email "bob@example.com",
           :dustingetz/shirt-size
           [:select
            {:value :dustingetz/mens-large}
            [[:option :dustingetz/mens-small]
             [:option :dustingetz/mens-medium]
             [:option :dustingetz/mens-large]]],
           :db/id 10}
          {:dustingetz/gender #:db{:ident :dustingetz/male},
           :dustingetz/email "charlie@example.com",
           :dustingetz/shirt-size
           [:select
            {:value :dustingetz/mens-medium}
            [[:option :dustingetz/mens-small]
             [:option :dustingetz/mens-medium]
             [:option :dustingetz/mens-large]]],
           :db/id 11}]})

;;;;;;;;;;;;;;;;;
;; FREE INPUTS ;;
;;;;;;;;;;;;;;;;;

(p/def constant-input #'"")

(tests
  (p/run (! (p/binding [render-input constant-input]
              (hfql [{(submissions .) [:db/id]}]) )))
  %)

;; [(:dustingetz/shirt-size ::hf/render render-typeahead
;;                          ::hf/options (shirt-sizes gender .))
;;  {:dustingetz/gender [(:db/ident ::hf/as gender)]}]

(comment
  (hyperfiddle.rcf/set-timeout! 5000)

  ;; (hfql [{:dustingetz/gender [(:db/ident ::hf/as gender)]}])

  ;; (hfql [(:dustingetz/shirt-size ::hf/options (shirt-sizes gender nil))])

  (hfql [(:dustingetz/shirt-size ::hf/options (shirt-sizes gender nil))
         {:dustingetz/gender [(:db/ident ::hf/as gender)]}])

  ;; 1. collect ::hf/as
  ;; 2. collect paths to ::hf/as
  ;; 3. collect references to ::hf/as
  ;; 4. introduce bindings from ::hf/as to paths to ::hf/as

  )


;; ))
