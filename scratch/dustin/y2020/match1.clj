(ns dustin.match2)

(def *type-registry (atom {}))

(defmacro typedef [T]
  (swap! *type-registry assoc T {})
  `(def T ^{:type T} {}))

(typedef A)



(declare match | >> _)

(let [instance ^{:type `A} {::a 42}]
  (match instance
    | (A 42) = nil
    | (::B (::A 42)) = nil
    | (::B (::A _)) = nil
    | _ = nil))


(defn sign [x]
  (match x
    :| (> x 0) := 1
    :| (= x 0) := 0
    :| (< x 0) := -1
    ))
