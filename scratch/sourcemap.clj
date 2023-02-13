(ns hfdl.sourcemap
  "experiment to take a photon IR and map back to original code; used by viz to render nodes as sexprs"
  (:import (hfdl.impl.compiler Dataflow)))

#_(defn decompile [program]
  (reduce (fn [asts [op & args]]
            (conj asts
                  (case op
                    :apply           (cons (asts (first args)) (map asts (second args)))
                    :constant        (list `clojure.core/unquote (asts (first args)))
                    :variable        (list `clojure.core/deref (asts (first args)))
                    (:local :global) (first args))))
          []
          (:graph program)))

(defn decompile* [[op & args]]
  (case op
    :remote   (list `clojure.core/unquote-splicing (decompile* (first args)))
    :constant (list `clojure.core/unquote (decompile* (first args)))
    :variable (list `clojure.core/deref (decompile* (first args)))
    :apply    (cons (decompile* (first args)) (map decompile* (second args)))
    :local    (list `quote (first args))
    :global   (first args)))

(defn decompile [program]
  (decompile* (:expression program)))

#_(tests
 (require '[hfdl.lang :refer [dataflow debug! heap-dump]]
          '[clojure.data :refer [diff]]
          '[minitest :refer [tests]]
          '[missionary.core :as m]))

#_(tests
 (defmacro recover [ast]
   `(decompile (dataflow ~ast)))

 (declare a f)
 (recover a) := `[a]
 (recover @a) := `[a @a]
 (recover (f @a)) := `[a @a f (f @a)]
 (recover [(f @a) (f @a)]) := `[vector a @a f (f @a) (vector (f @a) (f @a))]
 ;; Decompiler don't see in child programs
 (def child (dataflow @a))
 (recover [@child]) := `[vector child @child (vector @child)]
 )

(defn locate [program form]
  (->> (decompile program)
       (map-indexed vector)
       (filter (fn [[_idx form']] (= form form'))) ; .indexOf
       (ffirst)))

#_(tests
 (declare a f)
 (def program (dataflow (f (str a) (str a))))

 (locate program `a) := 0
 (locate program `str) := 1
 (locate program `(str a)) := 2
 (locate program `f) := 3
 (locate program `(f (str a) (str a))) := 4)

(defn subpath? [path-a path-b]
  (and (< (count path-a) (count path-b))
       (->> (map vector path-a path-b)
            (drop-while (fn [[a b]] (= a b)))
            (empty?))))

(defn focus [m path]
  (->> (seq m)
       (filter (fn [[k _v]] (subpath? path k)))
       (mapv (fn [[k v]] [(vec (next k)) (vec (nnext k)) v]))
       (group-by ffirst)
       (map (fn [[k vs]] [k (into {} (map (fn [v] (vec (next v))) vs))]))
       (into {})
       (not-empty)))

(defn has-nested-frame? [heap ip]
  (->> (keys heap)
       (filter (fn [path] (subpath? ip path)))
       (not-empty)))

(defn top-frame [heap]
  (into {} (filter (fn [[k _v]] (= 1 (count k))) heap)))

(declare humanize)

(defn- humanize* [intermediate-lang decompiled heap]
  (reduce-kv (fn [r k v]
               (let [source-form (get-in decompiled k)]
                 (if (instance? Dataflow v)
                   (if-not (contains? r source-form)
                     (assoc r source-form v) ; will be overwritten later if variable
                     r)
                   (let [[type & args] (get-in intermediate-lang k)
                         source-form   (get-in decompiled k)]
                     (case type
                       :variable (let [k⁻¹ [(first args)]
                                       v⁻¹ (get heap k⁻¹)]
                                   (if (instance? Dataflow v⁻¹)
                                     (let [r' (->> (focus heap k)
                                                   (reduce-kv (fn [r fork-id nested-heap] (assoc r fork-id (humanize v⁻¹ nested-heap)))
                                                              {})
                                                   (assoc r (get-in decompiled k⁻¹)))]
                                       (assoc r' source-form v))
                                     (assoc r source-form v)))
                       (assoc r source-form v))))))
             {}
             (top-frame heap)) )

(defn humanize
  "Correlate a heap dump with the original program."
  ([program] (let [intermediate-lang (:graph program)
                   decompiled        (decompile program)]
               (fn [heap]
                 (humanize* intermediate-lang decompiled heap))))
  ([program heap]
   (humanize* (:graph program) (decompile program) heap)))

#_(tests
 (def !a (atom 0))
 (def a (m/watch !a))
 (def plus-1 inc) ; prevent inlining
 (def minus-1 dec)
 (def program (dataflow [(plus-1 @a) (minus-1 @a)]))
 (def process (debug! program))

 (def heap (heap-dump @process))
 (def expected-heap {[0] minus-1,
                     [1] a,
                     [2] 0,
                     [3] -1,
                     [4] vector,
                     [5] plus-1,
                     [6] 1,
                     [7] [1 -1]})
 heap := expected-heap
 (diff heap expected-heap) := [nil nil expected-heap] ; useful to diagnose inlining

 (def expected {`vector                            vector,
                `a                                 a,
                `@a                                0,
                `plus-1                            plus-1,
                `(plus-1 @a)                       1,
                `minus-1                           minus-1,
                `(minus-1 @a)                      -1,
                `(vector (plus-1 @a) (minus-1 @a)) [1 -1]})


 (def humanized (humanize program (heap-dump @process)))
 humanized := expected
 (diff humanized expected) := [nil nil expected] ; useful to diagnose inlining
 )

#_(tests ; nested frames
 (def !a (atom 0))
 (def a (m/watch !a))
 (def grandchild (dataflow (str @a)))
 (def child (dataflow (str @grandchild @grandchild)))
 (def program (dataflow [@child @child]))
 (def process (debug! program))

 (def heap (heap-dump @process))
 heap := {[0]         vector,
          [1]         child,
          [2]         "00",
          [3]         ["00" "00"],
          [2 0 0]     str,
          [2 0 1]     grandchild,
          [2 0 2]     "0",
          [2 0 3]     "00",
          [2 0 2 0 0] a,
          [2 0 2 0 1] 0,
          [2 0 2 0 2] str,
          [2 0 2 0 3] "0"}

 (def humanized (humanize program heap))
 (def expected {`vector                 vector,
                `child                  {0 {`str                           str
                                            `grandchild                    {0 {`a        a,
                                                                               `@a       0,
                                                                               `str      str
                                                                               `(str @a) "0",}},
                                            `@grandchild                   "0",
                                            `(str @grandchild @grandchild) "00"}},
                `@child                 "00"
                `(vector @child @child) ["00" "00"]})
 humanized := expected
 (diff humanized expected) := [nil nil expected] ; convenient to diagnose
 )

#_(tests ; manually merging streams
 (def !a (atom 0))
 (def a (m/watch !a))
 (def grandchild (dataflow (str @a)))
 (def child (dataflow (str @grandchild)))
 (def program (dataflow @(m/latest vector child grandchild)))
 (def process (debug! program))

 (def heap (heap-dump @process))

 heap := {[0]         vector,
          [1]         _ ;grandchild. Minitest error => Can't create empty: Dataflow
          [2]         _ ;child.      Minitest error => Can't create empty: Dataflow
          [3]         m/latest,
          [4]         _
          [5]         ["0" "0"]
          [5 0 0]     str,
          [5 0 1]     _ ;grandchild. Minitest error => Can't create empty: Dataflow
          [5 0 2]     "0",
          [5 0 3]     "0",
          [5 1 0]     a
          [5 1 1]     0
          [5 1 2]     str
          [5 1 3]     "0"
          [5 0 2 0 0] a
          [5 0 2 0 1] 0
          [5 0 2 0 2] str
          [5 0 2 0 3] "0"}

 (def humanized (humanize program heap))

 humanized := {`child                               _ ;child.      Minitest error => Can't create empty: Dataflow
               `grandchild                          _ ;grandchild. Minitest error => Can't create empty: Dataflow
               `vector                              vector
               `m/latest                            m/latest
               `(m/latest vector child grandchild)  _
               `@(m/latest vector child grandchild) ["0" "0"]}
 )

