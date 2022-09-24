(ns contrib.data
  (:require clojure.math
            [hyperfiddle.rcf :refer [tests]]))

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
  {:pre [(or (nil? ?qualified-kw)
             (qualified-keyword? ?qualified-kw))]}
  (if ?qualified-kw
    (keyword (name ?qualified-kw))))

(tests
  (unqualify ::x) := :x
  (unqualify nil) := nil
  ;(unqualify "") :throws #?(:clj AssertionError :cljs js/Error)
  )

(defn omit-keys-ns [ns ?m]
  {:pre [(some? ns)]}
  (when ?m
    (reduce-kv (fn [m k v] (if (= (name ns) (namespace k))
                             m (assoc m k v))) {} ?m)))

(tests
  (omit-keys-ns :c {::a 1 :b 2 :c/c 3}) := {::a 1 :b 2}
  (omit-keys-ns :c {::a 1 :b 2 :c/c 3}) := {::a 1 :b 2}
  (omit-keys-ns :c nil) := nil
  ;(omit-keys-ns nil {::a 1 :b 2 :c/c 3}) :throws #?(:clj AssertionError :cljs js/Error)
  ;(omit-keys-ns nil nil) :throws #?(:clj AssertionError :cljs js/Error)
  )

(defn auto-props "qualify any unqualified keys to the current ns and then add qualified defaults"
  [ns props defaults-qualified]
  (merge defaults-qualified (update-keys props (partial qualify ns))))

(tests
  (auto-props (namespace ::this) {:a 1 ::b 2} {::b 0 ::c 0}) := {::a 1 ::b 2 ::c 0})

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
  (round-floor 16 8) := 16.0)
