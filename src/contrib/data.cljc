(ns contrib.data
  (:require clojure.math
            [clojure.datafy :refer [datafy]] ; todo remove
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [contrib.data :refer [auto-props]])))

(defn qualify
  "Qualify a keyword with a namespace. If already qualified, leave kw untouched. Nil-safe.
  (qualify :db :isComponent) -> :db/isComponent"
  [ns ?kw]
  {:pre [(some? ns) #_(not (namespace ns)) (keyword? ?kw)]}
  (when ?kw
    (if (qualified-keyword? ?kw)
      ?kw (keyword (name ns) (name ?kw)))))

(tests
  ;(keyword (name (namespace ::ns)) (name :limit))
  (qualify (namespace ::x) :limit) := ::limit
  ;(qualify (namespace ::x) "limit") thrown? AssertionError
  "leave qualified kws untouched"
  (qualify (namespace ::x) :user/foo) := :user/foo)

(defn unqualify
  "Strip namespace from keyword, discarding it and return unqualified keyword. Nil-safe.
  (unqualify :db.type/ref) -> :ref"
  [?qualified-kw]
  (assert (or (nil? ?qualified-kw)
              (qualified-keyword? ?qualified-kw)) (str " can't unqualify: " ?qualified-kw))
  (if ?qualified-kw
    (keyword (name ?qualified-kw))))

(tests
  (unqualify ::x) := :x
  (unqualify :db.type/ref) := :ref
  (unqualify nil) := nil
  (unqualify "") :throws #?(:clj AssertionError :cljs js/Error))

(defn -omit-keys-ns [ns ?m]
  {:pre [(some? ns)]}
  (when ?m
    (reduce-kv (fn [m k v] (if (= (name ns) (namespace k))
                             m (assoc m k v))) {} ?m)))

(defmacro omit-keys-ns
  ([?m] `(-omit-keys-ns ~(str *ns*) ~?m))
  ([ns- ?m] `(-omit-keys-ns ~ns- ~?m)))

(tests
  (omit-keys-ns :c {::a 1 :b 2 :c/c 3}) := {::a 1 :b 2}
  (omit-keys-ns :c {::a 1 :b 2 :c/c 3}) := {::a 1 :b 2}
  (omit-keys-ns :c nil) := nil
  (omit-keys-ns nil {::a 1 :b 2 :c/c 3}) :throws #?(:clj AssertionError :cljs js/Error)
  (omit-keys-ns nil nil) :throws #?(:clj AssertionError :cljs js/Error)
  nil)

(defn has-ns?
  "State if a `named` value (keyword or symbol) has such namespace `ns`.
  `ns` can be be a string, or a non-namespaced keyword or symbol."
  [ns named]
  {:pre [(or (string? ns) (simple-ident? ns))]}
  (= (name ns) (namespace named)))

(defn select-ns
  "Like `select-keys` but select all namespaced keys by ns."
  [ns map]
  (into (empty map) (filter (fn [[k _v]] (has-ns? ns k))) map))

(defn -auto-props "qualify any unqualified keys to the current ns and then add qualified defaults"
  [ns props defaults-qualified]
  {:pre [(some? ns) (or (string? ns) (symbol? ns))]}
  (merge defaults-qualified (update-keys props (partial qualify ns))))

(defmacro auto-props
  ([ns props defaults-qualified] `(-auto-props ~ns ~props ~defaults-qualified))
  ([props defaults-qualified] `(-auto-props ~(str *ns*) ~props ~defaults-qualified))
  ([props] `(-auto-props ~(str *ns*) ~props {})))

(tests
  (auto-props "user" {:a 1} {:dom/class "a"}) := {:user/a 1 :dom/class "a"}
  (auto-props 'user {:a 1} {:dom/class "a"}) := {:user/a 1 :dom/class "a"}
  (auto-props *ns* {:a 1} {:dom/class "a"}) :throws #?(:clj AssertionError :cljs js/Error)
  (auto-props {:a 1} {:dom/class "a"}) := {:contrib.data/a 1 :dom/class "a"}
  (auto-props {:a 1}) := {:contrib.data/a 1})

(defn xorxs
  "an argument parser that accepts both scalars and collections, lifting scalars into a collection"
  [xorxs & [zero]]
  (cond (vector? xorxs) xorxs
        (set? xorxs) xorxs
        (seq? xorxs) xorxs
        (nil? xorxs) zero
        :else-single-value-or-map (conj (or zero []) xorxs)))

(tests
  (xorxs :a)     := [:a]
  (xorxs [:a])   := [:a]
  (xorxs #{:a})  := #{:a}
  (xorxs :a #{}) := #{:a}
  (xorxs :a [])  := [:a]
  (xorxs nil #{}) := #{}
  (xorxs nil) := nil)

(defn index-by [kf xs]
  {:pre [kf]}
  (into {} (map-indexed (fn [i x]
                          [(kf x i) ; fallback to index when key is not present
                           #_(if-not kf (kf x i) i) ; alternative design is to define nil kf as fallback
                           x])) xs))

(tests
  (def xs [{:db/ident :foo :a 1}
           {:db/ident :bar :b 2}])

  (index-by :db/ident xs)
  := {:foo {:db/ident :foo, :a 1},
         :bar {:db/ident :bar, :b 2}}

  (index-by ::missing xs) ; should this throw?
  := {0 {:db/ident :foo, :a 1},
      1 {:db/ident :bar, :b 2}}

  ;"nil kf uses default value (which is likely unintended, should it throw?)"
  ;(index-by nil xs)
  ;:= {0 {:db/ident :foo, :a 1},
  ;    1 {:db/ident :bar, :b 2}}

  (index-by :a nil) := {}
  ;(index-by nil nil) := {} ; kf never used -- alternative design
  (index-by nil nil) :throws #?(:clj AssertionError :cljs js/Error)

  (index-by :a [{}]) := {0 {}}
  (index-by :a [{:a 1}]) := {1 {:a 1}}
  (index-by :b [{:a 1}]) := {0 {:a 1}} ; missing key, fallback to index

  "indexing map entries (which is weird, should this throw?)"
  (index-by :a {:a 1}) := {0 [:a 1]} ; index the map entry, not the map, :a is missing so fallback
  (index-by :b {:a 1}) := {0 [:a 1]}

  "collisions are possible"
  (index-by :db/id [{:db/id 1} {:db/id 2} {:db/id 1}]) ; should this detect collision and throw?
  := {1 #:db{:id 1}, 2 #:db{:id 2}}

  "kf fallback arity"
  (index-by (fn [x i] (str i)) xs)
  := {"0" {:db/ident :foo, :a 1},
      "1" {:db/ident :bar, :b 2}}

  "index by first element"
  ;(index-by first [[:a 1] [:b 2]]) -- ArityException: kf must accept fallback. Is this a mistake?
  (index-by (fn [a b] (first a)) [[:a 1] [:b 2]]) := {:a [:a 1], :b [:b 2]}
  (index-by #(do %2 (first %1)) [[:a 1] [:b 2]]) := {:a [:a 1], :b [:b 2]})

(defn index
  "index a sequential collection into an associative collection with explicit keys. this may not be
  useful, as vectors are already associative"
  [xs]
  (assert (sequential? xs)) ; maps are not indexable
  (index-by (fn [x i] i) xs))

(tests
  (def xs [{:db/ident :foo :a 1}
           {:db/ident :bar :b 2}])
  (index xs)
  := {0 {:db/ident :foo, :a 1},
      1 {:db/ident :bar, :b 2}})

(defn group-by-pred [f? xs] ; todo rename
  (let [{a true b false} (group-by f? xs)]
    [a b]))

(tests
  (group-by-pred map? [:user/email
                       {:user/gender [:db/ident]}
                       {:user/shirt-size [:db/ident]}
                       :db/id])
  := [[#:user{:gender [:db/ident]}
       #:user{:shirt-size [:db/ident]}]
      [:user/email
       :db/id]])

(defn update-existing [m k f & args]
  (if (get m k)
    (apply update m k f args)
    m))

(tests
  (update-existing {:a 1} :a + 10) := {:a 11}
  (update-existing {:a 1} :b + 10) := {:a 1})

;(defn positional
;  "Transform an array-like map {0 :foo, 1 :bar, ...} with contiguous array keys (0, 1, ...) into
;   list [:foo :bar]"
;  [amap]
;  (->> (range (inc (count amap)))
;       (reduce (fn [acc idx]
;                 (if (contains? amap idx)
;                   (conj acc (get amap idx))
;                   (reduced acc)))
;               [])
;       (seq)))
;
;(tests (positional {0 :foo 1 :bar}) := [:foo :bar])

;; https://github.com/weavejester/medley/blob/master/src/medley/core.cljc
;; https://clojure.atlassian.net/browse/CLJ-1451
(defn take-upto [pred]
  (fn [rf] 
    (fn 
      ([] (rf))
      ([ac] (rf ac))
      ([ac nx] (cond-> (rf ac nx) (pred nx) ensure-reduced)))))

(tests
  (into [] (take-upto odd?) [2 4 6 8 9 10 12 14]) := [2 4 6 8 9])

(defn round-floor [n base] (* base (clojure.math/floor (/ n base))))

(comment
  "base 10"
  (round-floor 89 10) := 80.0
  (round-floor 90 10) := 90.0
  (round-floor 91 10) := 90.0
  (round-floor 99 10) := 90.0
  (round-floor 100 10) := 100.0
  "base 8"
  (round-floor 7 8) := 0.0
  (round-floor 8 8) := 8.0
  (round-floor 9 8) := 8.0
  (round-floor 15 8) := 8.0
  (round-floor 16 8) := 16.0

  (round-floor 1234567 1000) := 1234000.0)

(defn pad
  ([zero coll] (concat coll (repeat zero)))
  ([n zero coll] (take n (pad zero coll))))

(defn padl [n zero coll] (concat (repeat (- n (count coll)) zero) coll))

(tests
  (pad 8 0 (range 3)) := [0 1 2 0 0 0 0 0]
  (padl 8 0 (range 3)) := [0 0 0 0 0 0 1 2]

  "strings leak platform internals, use padl-str"
  (pad 8 "0" "xx") := #?(:clj [\x \x "0" "0" "0" "0" "0" "0"]
                         :cljs ["x" "x" "0" "0" "0" "0" "0" "0"])

  (padl 8 "0" "xx") := #?(:clj ["0" "0" "0" "0" "0" "0" \x \x]
                          :cljs ["0" "0" "0" "0" "0" "0" "x" "x"]))

(defn assoc-vec [xs k v]
  (if (>= k (count xs))
    (assoc (vec (pad k nil xs)) k v)
    (assoc xs k v)))

(tests
  (assoc-vec [] 0 :a) := [:a]
  (assoc-vec [] 1 :b) := [nil :b]
  (assoc-vec [] 4 :e) := [nil nil nil nil :e]
  (assoc-vec nil 4 :e) := [nil nil nil nil :e]
  (assoc-vec [:a :b :c] 1 :B) := [:a :B :c]
  (assoc-vec [:a :b :c] 4 :E) := [:a :b :c nil :E])

(defn padl-str [n zero s] (apply str (padl n zero s)))

(tests
  (padl-str 8 "0" "xx") := "000000xx"
  (padl-str 4 "0" (str 11)) := "0011")

(defn with-pad [reducer zero]
  (fn [f & cols]
    (let [n (apply max (map count cols))
          cols (map #(pad n zero %) cols)]
      (apply reducer f cols))))

(def map-pad (partial with-pad map))

(tests
  (map + [1 1 1] [1 1 1 1]) := '(2 2 2)
  ((map-pad 0) + [1 1 1] [1 1 1 1]) := '(2 2 2 1))

(defn str-last-n [n s]
  #?(:clj (.substring s (max 0 (- (.length s) n)))
     :cljs (apply str (reverse (take n (reverse s))))))

(tests
  (str-last-n 4 "0123456789") := "6789")

; org.apache.commons.lang3.StringUtils.containsIgnoreCase()
;(defn str-contains-ignore-case [])

;(defn clamp [n min max] (Math/min (Math/max n min) max))
;
;(tests
;  (clamp 51 10 50) := 50
;  (clamp 50 10 50) := 50
;  (clamp 49 10 50) := 49
;  (clamp 11 10 50) := 11
;  (clamp 10 10 50) := 10
;  (clamp 9  10 50) := 10)

#?(:clj
   (defmacro orp
     "`clojure.core/or` evaluates arguments one by one, returning the first truthy
  one and so leaving the remaining ones unevaluated. `orp` does the same but
  with a custom predicate."
     ([pred] nil)
     ([pred x]
      `(let [or# ~x]
         (when (~pred or#) or#)))
     ([pred x & next]
      `(let [or# ~x]
         (if (~pred or#) or# (orp ~pred ~@next))))))

(tests
  (orp some? nil false 1) := false
  (orp even? 1 3 5 6 7) := 6)

(defn nil-or-empty? [x]
  (if (seqable? x)
    (empty? x)
    (nil? x)))

(defn- -tree-list [depth xs children-fn keep? input]
  (eduction (mapcat (fn [x]
                      (let [x (datafy x)]
                        (if-let [children (children-fn x)]
                          (when-let [rows (seq (-tree-list (inc depth) children children-fn keep? input))]
                            (into [[depth x]] rows))
                          (cond-> [] (keep? x input) (conj [depth x]))))))
    (datafy xs)))

(defn treelister
  ([xs] (treelister xs (fn [_]) (fn [_ _] true)))
  ([xs children-fn keep?] (fn [input] (-tree-list 0 xs children-fn keep? input))))

(tests
  (vec ((treelister [1 2 [3 4] [5 [6 [7]]]] #(when (vector? %) %) (fn [v _] (odd? v))) nil))
  := [[0 1] [0 [3 4]] [1 3] [0 [5 [6 [7]]]] [1 5] [1 [6 [7]]] [2 [7]] [3 7]]

  ((treelister [{:dir "x" :children [{:file "a"} {:file "b"}]}] :children (fn [v needle] (-> v :file #{needle})) ) "a")
  (count (vec *1)) := 2

  "directory is omitted if there are no children matching keep?"
  ((treelister [{:dir "x" :children [{:file "a"} {:file "b"}]}] :children (fn [v needle] (-> v :file #{needle}))) "nope")
  (count (vec *1)) := 0)