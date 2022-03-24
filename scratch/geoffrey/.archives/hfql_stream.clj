(ns geoffrey.hfql-stream
  (:require [dustin.fiddle :refer :all]
            [dustin.hf-nav :refer :all]
            [dustin.monad-scope :as ms :refer [bind pure runScope sequence]]
            [hyperfiddle.fabric :as df]
            [meander.epsilon :as m :refer [match]]
            [minitest :refer [tests]]))

(defmacro trace
  ([sym]
   `(trace ~sym ~sym))
  ([tag sym]
   (let [{:keys [:line :column]} (meta &form)]
     `(fn [& args#]
        (prn '~tag ~(str "@" line ":" column) " := " args#)
        (apply ~sym args#)))))

(defmacro safe [f]
  `(fn [& args#]
     (try
       (apply ~f args#)
       (catch Exception err#
         (prn err#)
         (throw err#)))))

(defmacro with-input [value query]
  `(let [result#  (atom nil)
         ~'input (df/input)
         output#  ~query]
     (df/on output# (partial reset! result#))
     (df/put ~'input ~value)
     (if-let [val# @result#]
       val#
       output#)))


;; ---------------------------------------

(defn fmap [f & as]
  (cond
    (every? #(instance? hyperfiddle.View %) as) (apply df/fmap f as)
    (every? fn? as)                             (apply ms/fmap f as)
    :else                                       (apply f as)))

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(defn hf-nav* [edge a]
  ((safe hf-nav) edge a))

(defn hf-apply [edge a scope]
  (cond
    (keyword? edge) (fmap (partial hf-nav* edge) a)
    (= '* edge)     (fmap touch1 a)
    (seq? edge)     (let [[f & args] edge]
                      (apply fmap (safe (clojure.core/resolve f)) (replace scope args)))
    ()              (println "hf-eval unmatched edge: " edge)))

(defn hf-eval [edge a]
  (fn [scope]
    (let [b (hf-apply edge a scope)]
      [{(hf-edge->sym edge) b, '% b}
       b])))

(defn hf-pull' [pat ma]
  (match pat
    [!pats ...]
    (->> !pats
         (mapv (fn [pat] (hf-pull' pat ma)))
         (sequence)
         (fmap (fn [mas] (apply fmap merge mas))))

    {& (m/seqable [?edge ?pat])}
    (as-> ma ma
      (bind ma (partial hf-eval ?edge))
      (bind ma (fn [>a]
                (fn [scope]
                  [scope (df/fmap (fn [a]
                                 (if (vector? a)
                                   (second (runScope (sequence (mapv #(hf-pull' ?pat (pure %)) a)) scope))
                                   (second (runScope (hf-pull' ?pat (pure a)) scope))))
                                  >a)])))
      #_(bind ma (fn [>a] (df/put >a)))
      #_(bind ma (fn [>a]
                 (pure
                  (df/fmap (fn [a]
                             (if (vector? a)
                               (sequence (mapv #(hf-pull' ?pat (pure %)) a))
                               (hf-pull' ?pat (pure a))))
                           >a))))
      ;; (hf-pull' ?pat ma)
      (fmap (fn [>a] (fmap (fn [a] {?edge a}) >a)) ma))

    ?leaf
    (as-> ma ma
      (bind ma (partial hf-eval ?leaf))
      (fmap (fn [>a] (fmap (fn [a] {?leaf a}) >a)) ma))))

(defn hf-pull! [pat v scope]
  (let [[_ v] (runScope (hf-pull' pat (pure v)) scope)]
    v))

(defmacro hf-pull [pat v & [scope]]
  `(hf-pull! (quote ~pat) ~v ~(or scope {})))


;; --------------------------------------------------------------------------------

(tests

 (with-input :dustingetz/male
   (hf-pull (identity %) :dustingetz/male {'% input}))
 := '{(identity %) :dustingetz/male}

 (with-input 17592186045429
   (hf-pull :dustingetz/gender input))
 := '#:dustingetz{:gender :dustingetz/male}

 (with-input 17592186045429
   (hf-pull [{:dustingetz/gender [:db/ident]}] input))
 := '#:dustingetz{:gender #:db{:ident :dustingetz/male}}


 (with-input dustin.dev/*$*
   (hf-pull (gender $) nil {'$ input}))
 := '{(gender $) 17592186045418}

 (with-input 17592186045428
   (hf-pull :dustingetz/tags input {}))
 := #:dustingetz{:tags #{:c :b :a}}

 (with-input "alice"
   (hf-pull {(submission >needle) [:dustingetz/tags]} nil {'>needle input}))
 := {(submission >needle) #:dustingetz{:tags #{:c :b :a}}}

 (with-input "example"
   (hf-pull {(submissionS >needle) [:dustingetz/tags]} nil {'>needle input}))
 := {(submission >needle) #:dustingetz{:tags #{:c :b :a}}}

 (with-input "example"
   (hf-pull (submissionS >needle) nil {'>needle input}))
 := '{(submissionS needle) [17592186045428 17592186045429 17592186045430]}

 ;; TAG
 (with-input "example"
   (hf-pull {(submissionS >needle) [:dustingetz/gender]} nil {'>needle input}))
 := '{(submissionS needle) [#:dustingetz{:gender :dustingetz/female}
                            #:dustingetz{:gender :dustingetz/male}
                            #:dustingetz{:gender :dustingetz/male}]}

 (with-input "example"
   (hf-pull {(submissionS >needle) [{:dustingetz/gender [:db/ident]}]} nil {'>needle input}))
 := '{(submissionS needle) [#:dustingetz{:gender #:db{:ident :dustingetz/female}}
                            #:dustingetz{:gender #:db{:ident :dustingetz/male}}
                            #:dustingetz{:gender #:db{:ident :dustingetz/male}}]}

 (with-input "example"
   (hf-pull {(submissionS needle) {:dustingetz/gender [(shirt-size dustingetz/gender)]}} nil {'needle input}))
 := '{(submissionS needle) [#:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045425}}
                            #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045421}}
                            #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045421}}]}

 (with-input "example"
   (hf-pull {(submissionS needle) [{:dustingetz/gender [{(shirt-size dustingetz/gender) [:db/id]}]}]} nil {'needle input}))
 := '{(submissionS needle) [#:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:id 17592186045425}}}
                            #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:id 17592186045421}}}
                            #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:id 17592186045421}}}]}

 (with-input "example"
   (hf-pull
    {(submissionS needle)
     [{:dustingetz/gender
       [{(shirt-size dustingetz/gender)
         [:db/ident]}]}]}
    nil
    {'needle input}))

 := '{(submissionS needle) [#:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/womens-medium}}}
                            #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/mens-small}}}
                            #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/mens-small}}}]}

 (with-input 17592186045418
   (hf-pull [{:db/ident [:db/id]}] input))
 := #:db{:ident #:db{:id 17592186045418}}

 (with-input 17592186045421
   (hf-pull [{:dustingetz/gender [{:db/ident [:db/id]}]}] input))
 := #:dustingetz{:gender #:db{:ident #:db{:id 17592186045418}}}

 (with-input 17592186045421
   (hf-pull [{:dustingetz/gender [{:db/ident [{:db/ident [:db/ident]}]}]}] input))
 := #:dustingetz{:gender #:db{:ident #:db{:ident #:db{:ident :dustingetz/male}}}}

 (with-input 17592186045421
   (hf-pull [{:dustingetz/gender [:db/id]}] input))
 := #:dustingetz{:gender #:db{:id 17592186045418}}

 (with-input 17592186045421
   (hf-pull [{:dustingetz/gender [{:db/id [:db/id]}]}] input))
 := #:dustingetz{:gender #:db{:id #:db{:id 17592186045418}}}


 (with-input 17592186045421
   (hf-pull [{:dustingetz/gender [{:db/id [{:db/id [{:db/id [:db/id]}]}]}]}] input))
 := #:dustingetz{:gender #:db{:id #:db{:id #:db{:id #:db{:id 17592186045418}}}}}

 )
