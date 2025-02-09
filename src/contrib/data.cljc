(ns contrib.data
  (:refer-clojure :exclude [group-by])
  (:require clojure.math
            clojure.string
            [clojure.datafy :refer [datafy]] ; todo remove
            #_[contrib.str :refer [any-matches?]] ; cycle, inlined impl
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [contrib.data :refer [auto-props]])))

(defn -qualify [ns ?kw]
  {:pre [(some? ns) #_(not (namespace ns)) (keyword? ?kw)]}
  (when ?kw
    (if (qualified-keyword? ?kw) ?kw
      (keyword (name ns) (name ?kw)))))

(defmacro qualify "
Qualify a keyword with a namespace. If already qualified, leave untouched. Nil-safe.
  (qualify :db :isComponent) := :db/isComponent
  (qualify :isComponent) := ::isComponent"
  ([ns ?kw] `(-qualify ~ns ~?kw))
  ([?kw] `(-qualify ~(str *ns*) ~?kw)))

(tests
  ;(keyword (name (namespace ::ns)) (name :limit))
  (qualify :limit) := ::limit

  (qualify "contrib.data" :limit) := ::limit
  (qualify :contrib.data :limit) := ::limit
  (qualify (namespace ::x) :limit) := ::limit
  ;(qualify (namespace ::x) "limit") thrown? AssertionError
  "leave qualified kws untouched"
  (qualify :user/foo) := :user/foo)

(defn unqualify
  "Strip namespace from keyword, discarding it and return unqualified keyword. Nil-safe.
  (unqualify :db.type/ref) -> :ref"
  [?kw]
  (assert (or (nil? ?kw) (keyword? ?kw) (symbol? ?kw)) (str " can't unqualify: " ?kw))
  (cond
    (nil? ?kw) nil
    (keyword? ?kw) (keyword (name ?kw))
    (symbol? ?kw) (symbol (name ?kw))))

(tests
  (unqualify :db.type/ref) := :ref
  (unqualify ::x) := :x
  (unqualify :x) := :x
  (unqualify `x) := 'x
  (unqualify 'x) := 'x
  (unqualify "") :throws #?(:clj AssertionError :cljs js/Error)
  (unqualify nil) := nil)

(defn unqualified-keys [m] (update-keys m unqualify))

(tests
  (unqualified-keys {::a ::a}) := {:a ::a}
  (unqualified-keys {:a/x 1 :b/x 2}) := {:x 2} ; gotcha
  (unqualified-keys {}) := {}
  (unqualified-keys nil) := {}
  (unqualified-keys {nil ::a}) := {nil ::a}
  ;(unqualified-keys {1 ::a}) := {:a ::a} -- Assert failed:  can't unqualify: 1
  ;(unqualified-keys {{} ::a}) := {:a ::a} -- Assert failed:  can't unqualify: {}
  )

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

(defn omit-keys [m ks] (apply dissoc m ks))

(comment
  (omit-keys {::a 1 :b 2 :c/c 3} [::a]) := {:b 2 :c/c 3}
  )

(defn has-ns?
  "State if a `named` value (keyword or symbol) has such namespace `ns`.
  `ns` can be be a string, or a non-namespaced keyword or symbol."
  [?ns named]
  {:pre [(or (string? ?ns) (simple-ident? ?ns) (nil? ?ns))]}
  (= (some-> ?ns name) (some-> named namespace)))

(tests
  (has-ns? 'user :user/x) := true
  (has-ns? :user :user/x) := true
  (has-ns? "user" :user/x) := true
  (has-ns? 'user :x) := false
  (has-ns? 'user nil) := false
  ;(has-ns? *ns* ::x) := true -- crash, todo
  (has-ns? nil :user/x) := false
  (has-ns? nil :x) := true
  (has-ns? nil nil) := true)

(defn select-ns
  "Like `select-keys` but select all namespaced keys by ns."
  [ns map]
  (into (empty map) (filter (fn [[k _v]] (has-ns? ns k))) map))

(tests
  (select-ns 'user {:user/yo 1 :x 2}) := {:user/yo 1}
  (select-ns 'user {:x 2}) := {}
  (select-ns nil {:a 1 :user/b 2}) := {:a 1}
  (select-ns 'user nil) := nil)

(defn -auto-props "qualify any unqualified keys to the current ns and then add qualified defaults"
  [ns props defaults-qualified]
  {:pre [(some? ns) (or (string? ns) (symbol? ns))]}
  (merge defaults-qualified (update-keys props (partial -qualify ns))))

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

(defn map-entry [k v] (first {k v}))

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

(defn index-by
  "Given a key function `kf`, return a map of {(kf x) x} for all x in `xs`.
If (kf x) return nil, falls back to positional index.
(index-by kf) return a transducer producing pairs [(kf x) x]."
  ([kf]
   {:pre [kf]}
   (map-indexed (fn [i x]
                  [(kf x i) ; fallback to index when key is not present
                   x])))
  ([kf xs]
   (into {} (index-by kf) xs)))

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

(defn index-of "Find numeric index of `x` in seq `xs`, according to `=`.  O(n). Might never terminate if seq is infinite."
  [xs x]
  (loop [i 0
         xs (seq xs)]
    (cond (empty? xs) -1
          (= x (first xs)) i
          () (recur (inc i) (rest xs)))))

(defn group-by
  ([kf coll] (group-by kf (fnil conj []) coll))
  ([kf group-fn coll]
   (persistent!
     (reduce
       (fn [ret x]
         (let [k (kf x)]
           (assoc! ret k (group-fn (get ret k) x))))
       (transient {}) coll))))

(defn group-by-pred [f? xs] ; todo rename
  (let [{a true b false} (clojure.core/group-by f? xs)]
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

(defn subgroup-by [path-fn xs]
  (reduce (fn [acc x]
            (update-in acc (path-fn x) (fnil conj []) x))
    {} xs))

(tests
  (subgroup-by (juxt :category :type :brand) []) := {}
  (def xs [{:category "Electronics" :type "Laptop" :brand "Apple" :price 1200}
           {:category "Electronics" :type "Laptop" :brand "Dell" :price 900}
           {:category "Electronics" :type "Phone" :brand "Apple" :price 800}
           {:category "Books" :type "Fiction" :brand "Penguin" :price 15}
           {:category "Books" :type "NonFiction" :brand "O'Reilly" :price 40}
           {:category "Electronics" :type "Phone"}                   ; partial - 2 levels
           {:category "Books"}                                       ; partial - 1 level
           {:foo "bar"}])
  (subgroup-by (juxt :category :type :brand) xs)
  := {"Electronics" {"Laptop" {"Apple" [{:category "Electronics",
                                         :type "Laptop",
                                         :brand "Apple",
                                         :price 1200}],
                               "Dell" [{:category "Electronics",
                                        :type "Laptop",
                                        :brand "Dell",
                                        :price 900}]},
                     "Phone" {"Apple" [{:category "Electronics",
                                        :type "Phone",
                                        :brand "Apple",
                                        :price 800}],
                              nil [{:category "Electronics", :type "Phone"}]}},
      "Books" {"Fiction" {"Penguin" [{:category "Books", :type "Fiction", :brand "Penguin", :price 15}]},
               "NonFiction" {"O'Reilly" [{:category "Books",
                                          :type "NonFiction",
                                          :brand "O'Reilly",
                                          :price 40}]},
               nil {nil [{:category "Books"}]}},
      nil {nil {nil [{:foo "bar"}]}}})

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


(defn distinct-ordered [xs]
  (loop [remaining xs
         seen #{}
         result []]
    (if (empty? remaining)
      result
      (let [x (first remaining)]
        (recur (rest remaining)
          (conj seen x)
          (if (seen x)
            result
            (conj result x)))))))

(tests (distinct-ordered [1 2 1 3 2 4]) := [1 2 3 4])

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

(defn clamp-left [n left] (max n left)) ; when under limit, clamp up to larger
(defn clamp-right [n right] (min n right)) ; when exceeding limit, clamp down to smaller
(defn clamp [n left right] (clamp-right (clamp-left n left) right))

(tests
  (clamp 51 10 50) := 50
  (clamp 50 10 50) := 50
  (clamp 49 10 50) := 49
  (clamp 11 10 50) := 11
  (clamp 10 10 50) := 10
  (clamp 9  10 50) := 10)

(defn window [cnt offset limit xs]
  (subvec (vec xs) ; fast cast
    (max offset 0) (min (+ offset limit) cnt)))

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

(defn- any-matches? [coll needle] ; duplicate of contrib.str/any-matches?
  (let [substr (clojure.string/lower-case (str needle))]
    (some #(when % (clojure.string/includes? (clojure.string/lower-case (str %)) substr)) coll)))

(defn treelister
  ([xs] (treelister (fn [_]) any-matches? xs))
  ([children-fn xs] (treelister children-fn any-matches? xs)) ; don't make user :refer any-matches
  ([children-fn keep? xs] (fn [input] (-tree-list 0 xs children-fn keep? input))))

(tests
  (vec ((treelister #(when (vector? %) %) (fn [v _] (odd? v))
          [1 2 [3 4] [5 [6 [7]]]]) nil))
  := [[0 1] [0 [3 4]] [1 3] [0 [5 [6 [7]]]] [1 5] [1 [6 [7]]] [2 [7]] [3 7]]

  ((treelister :children (fn [v needle] (-> v :file #{needle}))
     [{:dir "x" :children [{:file "a"} {:file "b"}]}]) "a")
  (count (vec *1)) := 2

  "directory is omitted if there are no children matching keep?"
  ((treelister :children (fn [v needle] (-> v :file #{needle}))
     [{:dir "x" :children [{:file "a"} {:file "b"}]}]) "nope")
  (count (vec *1)) := 0)

(defn fn->
  ([f a] (fn [o] (f o a)))
  ([f a b] (fn [o] (f o a b)))
  ([f a b c] (fn [o] (f o a b c)))
  ([f a b c d] (fn [o] (f o a b c d)))
  ([f a b c d e] (fn [o] (f o a b c d e))))

(defn keep-if [v pred] (when (pred v) v))

(defn ->box
  ([] (->box nil))
  ([init] (let [o (doto (object-array 1) (aset (int 0) init))]
            (fn box
              ([] (aget o (int 0)))
              ([v] (aset o (int 0) v))
              ([retf swapf] (let [v (box), ret (retf v)] (box (swapf v)) ret))))))
