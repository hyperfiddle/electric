(ns dustin.y2020.expr
  (:require [clojure.spec.alpha :as s]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros dustin.y2020.expr)))

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

(defn over "traverse one layer" [x f]
  (cond (seq? x) (map f x)
        (sequential? x) (into (empty x) (map f x))
        (map? x) (into (empty x) (map (fn [[k v]] (f [k v])) x))
        ;(map? x) (into (empty x) (map (fn [[k v]] [(f k) (f v)]) x))
        () (throw (ex-info "cannot traverse" {:type (type x) :val  x}))))

(tests
  (over {:a 1 :b 2} (fn [[k v]] [k (inc v)])) := {:a 2, :b 3}
  (over (range 3) inc) := [1 2 3]
  (over '(1 2 (3 4)) println)
  )

(defn unquote? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote)))

(defn unquote-splicing? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote-splicing)))

(defn form? [x]
  (and (seq? x) ; yep, i bet you're surprised
    (not (empty? x))))

(tests
  (form? '(list)) := true
  (form? '#()) := true
  (form? '()) := false
  (form? 1) := false
  (form? {}) := false
  (form? []) := false)

(defn expr? [x]
  (or (symbol? x)
    (form? x)
    (and (coll? x)
      (not (empty? x)))))

(tests
  ; Not sure what GT's intent was here?
  (expr? 'a) := true
  (expr? 1) := false
  (expr? '(+ 1 1)) := true
  (expr? []) := false
  (expr? [1]) := true)

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

(tests
  (one [1]) := 1
  (try (one []) (catch #?(:clj Exception :cljs :default) _ :fail)) := :fail
  (try (one '(1 2)) (catch #?(:clj Exception :cljs :default) _ :fail)) := :fail

  ; questionable cases
  (one {:a 1}) := [:a 1]
  (try (one {:a 1 :b 2}) (catch #?(:clj Exception :cljs :default) _ :fail)) := :fail)

(defn traverse-expr [f x & {:keys [when-fn]}]
  (cond ((or when-fn expr?) x)
        (cond (symbol? x) (f x)
              () (over x f))
        () x))

(comment
  (traverse-expr println '(x 2 (y 3 4)))
  )

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

(comment
  (traverse-expr-rec inc (range 10))
  (traverse-expr-rec println '(x 2 (y 3 4)))
  (clojure.walk/postwalk identity '(1 2 (3 4)))

  )

#?(:clj
   (tests
     (traverse-expr-rec
       (fn [x] (if (symbol? x) (symbol #?(:clj (resolve x))) x))
       '(inc (dec 1)))
     := '(clojure.core/inc (clojure.core/dec 1))))

#?(:clj
   (defn unquote-via* [form f]
     (cond
       (symbol? form) `'~form
       ;(symbol? form) `'~(resolve form)
       (unquote? form) `'~(f (eval (unquote-via* (second form) f)))
       (unquote-splicing? form) (throw (Exception. "splice not in list"))
       (record? form) `'~form
       (coll? form)
       (let [xs (if (map? form) (apply concat form) form)
             parts (for [x xs]
                     (if (unquote-splicing? x)
                       (f (eval (unquote-via* (second form) f)))
                       [(unquote-via* x f)]))
             cat (doall `(concat ~@parts))]
         (cond
           (vector? form) `(vec ~cat)
           (map? form) `(apply hash-map ~cat)
           (set? form) `(set ~cat)
           (seq? form) `(apply list ~cat)
           :else (throw (Exception. "Unknown collection type"))))
       :else `'~form)))

#?(:clj
   (defn unquote-via [form f]
     (eval (unquote-via* form f))))

#?(:clj
   (tests
     (eval (unquote-via* '(println ~(inc 1)) identity)) := '(println (inc 1))
     (unquote-via '(println ~(inc 1)) identity) := '(println (inc 1))
     (unquote-via '(println ~(inc 1)) eval) := '(println 2)
     (unquote-via '[::find ~a] {'a 10}) := [::find 10]
     (def a 42)
     (unquote-via '{z ~a} eval) := '{z 42}))
