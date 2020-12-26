(ns fixplz.expr
  (:require
    [clojure.spec.alpha :as s])
  #?(:cljs (:require-macros [contrib.expr])))

(defn read-spec [spec input]
  ; Convenience for (do (s/assert spec x) (s/conform spec x))
  (let [result (s/conform spec input)]
    (when (s/invalid? result)
      (throw (ex-info (s/explain-str spec input)
               {:input input :spec spec})))
    result))

(defmacro read-vec [& xs]
  `(s/& (s/cat ~@(interleave (map (comp keyword str) (range 1 10)) xs))
     (s/conformer (comp vec vals))))

(defmacro read-alt [& xs]
  `(s/& (s/alt ~@(interleave (repeat :_) xs))
     (s/conformer (fn [v#] (second v#)))))


(defn spec-result [x]
  (cond (and (vector? x)
          (= (count x) 2)
          (keyword? (first x)))
        (cond (re-matches #"[_].*" (-> x first name)) (spec-result (second x))
              (re-matches #"[*].*" (-> x first name)) (map spec-result (second x))
              () x)
        (map? x)
        (cond (= (keys x) [:_]) (:_ x)
              () (into {} (map (fn [[k v]] [k (spec-result v)]) x)))
        () x))

(defn map-spec [f spec]
  (s/and spec (s/conformer f)))

(defn over [x f]
  (cond (seq? x) (map f x)
        (sequential? x) (into (empty x) (map f x))
        (map? x) (into (empty x) (map (fn [[k v]] (f [k v])) x))
        ;(map? x) (into (empty x) (map (fn [[k v]] [(f k) (f v)]) x))
        () (throw (ex-info "cannot traverse" {:type (type x) :val  x}))))

(defn unquote? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote)))

(defn unquote-splicing? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote-splicing)))

(defn form? [x]
  (and (seq? x) ; yep, i bet you're surprised
    (not (empty? x))))

(defn expr? [x]
  (or (symbol? x)
    (form? x)
    (and (coll? x)
      (not (empty? x)))))

;(defn stmt? [x]
;  (and (form? x)
;       (let [s (first x)]
;         (boolean
;           (or ('#{def let if do fn} s)
;               (some-> s resolve meta :macro))))))
;(defn user-expr? [x]
;  (and (expr? x)
;       (seq? x)
;       (not (stmt? x))))

(defn one [v]
  (if (= 1 (count v))
    (first v)
    (throw (ex-info (str "expecting one value in " v) {:v v})))
  (when v
    (assert (= 1 (count v)))
    (first v)))

(defn traverse-expr [f x & {:keys [when-fn]}]
  (cond ((or when-fn expr?) x)
        (cond (symbol? x) (f x)
              () (over x f))
        () x))

(defn traverse-expr-rec [f x & args]
  (apply
    traverse-expr
    (fn f' [v]
      (f (if (or (form? v)
               (coll? v))
           (apply traverse-expr f' v args)
           v)))
    x
    args))

(defn unquote-via [form f]
  (cond
    (symbol? form) `'~form
    ;(symbol? form) `'~(resolve form)
    (unquote? form) `'~(f (eval (unquote-via (second form) f)))
    (unquote-splicing? form) (throw (Exception. "splice not in list"))
    (record? form) `'~form
    (coll? form)
    (let [xs (if (map? form) (apply concat form) form)
          parts (for [x xs]
                  (if (unquote-splicing? x)
                    (f (eval (unquote-via (second form) f)))
                    [(unquote-via x f)]))
          cat (doall `(concat ~@parts))]
      (cond
        (vector? form) `(vec ~cat)
        (map? form) `(apply hash-map ~cat)
        (set? form) `(set ~cat)
        (seq? form) `(apply list ~cat)
        :else (throw (Exception. "Unknown collection type"))))
    :else `'~form))

(comment

  (eval (unquote-via '(println ~(inc 1)) identity))
  => (println (inc 1))

  (eval (unquote-via '(println ~(inc 1)) eval))
  => (println 2)

  (def scope {'a 10})
  (eval (unquote-via '[::find ~a] (fn [x] (get scope x))))
  => [::find 10]

  (def a 42)
  (eval (unquote-via '{z ~a} eval))
  => {z 42}

  )
