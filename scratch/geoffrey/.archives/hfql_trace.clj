(ns geoffrey.hfql16
  "macro wrapper for syntax"
  (:require [dustin.fiddle :refer :all]
            [dustin.hf-nav :refer :all]
            [dustin.monad-scope :refer [bind fmap pure runScope sequence]]
            [hyperfiddle.via :refer [Do-via via !]]
            [meander.epsilon :as m :refer [match]]
            [minitest :refer [tests]]))

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(defn hf-apply [edge a scope]
  (cond
    (keyword? edge) (hf-nav edge a)
    (= '* edge)     (touch1 a)
    (seq? edge)     (let [[f & args] edge]
                      (apply (resolve f) (replace scope args)))
    ()              (println "hf-eval unmatched edge: " edge)))

(defn trace! [scope expr result]
  (when (seq? expr)
    (let [form (-> (dissoc scope '$) ; we want $ to stay symbolic
                   (replace expr) ; partial eval
                   )]
      (! :Writer.log [form result])
      result)))

(defn hf-eval [edge a]
  (fn [scope]
    (let [b (hf-apply edge a scope)]
      (trace! scope edge b)
      [{(hf-edge->sym edge) b, '% b} b])))

(defn hf-pull' [pat ma]
  (match pat

    [!pats ...]
    (->> !pats
         (mapv (fn [pat] (hf-pull' pat ma)))
         (sequence)
         (fmap (fn [mas] (apply merge mas))))

    {& (m/seqable [?edge ?pat])}
    (as-> ma ma
      (bind ma (partial hf-eval ?edge))
      (bind ma (fn [a] (if (vector? a)
                        (sequence (mapv #(hf-pull' ?pat (pure %)) a))
                        (hf-pull' ?pat (pure a)))))
      (bind ma (fn [a]
                 (fn [scope]
                   (trace! scope ?edge a)
                   [scope {?edge a}]))))

    ?leaf
    (as-> ma ma
      (bind ma (partial hf-eval ?leaf))
      (fmap (fn [a] {?leaf a}) ma))))

(defn hf-pull! [pat v scope]
  (let [[_ v] (runScope (hf-pull' pat (pure v)) scope)]
    v))

(defmacro hf-pull [pat v & [scope]]
  `(hf-pull! (quote ~pat) ~v ~(or scope {})))

(deftype Writer [state]
  Do-via
  (resolver-for [_]
    {:Writer.log   (fn [x] (swap! state conj x))
     :Writer.state (fn [] @state)}))

(defmacro trace [& body]
  `(via (->Writer (atom []))
        ~@body
        (! :Writer.state)))

(defn partitions [trace] (into {} trace))

(tests

 (do (def ma (pure :dustingetz/male)) nil) := nil
 (runScope ma {})
 := [{} :dustingetz/male]

 (runScope (hf-pull' :db/ident ma) {})
 := '[{} #:db{:ident :dustingetz/male}]


 (partitions
  (trace
   (hf-pull (hf-nav :db/ident %) :dustingetz/male {'% :dustingetz/male})))
 := {'(hf-nav :db/ident :dustingetz/male) :dustingetz/male}

 (partitions
  (trace
   (hf-pull (identity %) :dustingetz/male {'% :dustingetz/male})))
 := '{(identity :dustingetz/male) :dustingetz/male}

 (hf-pull :dustingetz/gender 17592186045429)
 := '#:dustingetz{:gender :dustingetz/male}

 (hf-pull [{:dustingetz/gender [:db/ident]}] 17592186045429)
 := '#:dustingetz{:gender #:db{:ident :dustingetz/male}}


 (partitions
  (trace
   (hf-pull (gender $) nil {'$ dustin.dev/*$*})))
 := '{(gender $) 17592186045418}


 (partitions
  (trace
   (hf-pull :dustingetz/tags 17592186045428 {})))
 := {}

 ;; TAG
 (partitions
  (trace
   (hf-pull (submissionS needle) nil {'needle "example"})))
 := '{(submissionS "example") [17592186045428 17592186045429 17592186045430]}

 (partitions
  (trace
   (hf-pull {(submissionS needle) [:dustingetz/gender]}  nil {'needle "example"})))
 := '{(submissionS "example") [#:dustingetz{:gender :dustingetz/female}
                               #:dustingetz{:gender :dustingetz/male}
                               #:dustingetz{:gender :dustingetz/male}]}

 (partitions
  (trace
   (hf-pull {(submissionS needle) [{:dustingetz/gender [:db/ident]}]} nil {'needle "example"})))
 := '{(submissionS "example") [#:dustingetz{:gender #:db{:ident :dustingetz/female}}
                               #:dustingetz{:gender #:db{:ident :dustingetz/male}}
                               #:dustingetz{:gender #:db{:ident :dustingetz/male}}]}

 (partitions
  (trace
   (hf-pull {(submissionS needle) {:dustingetz/gender (shirt-size dustingetz/gender)}} nil {'needle "example"})))
 := '{(submissionS "example")         [#:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045425}}
                                       #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045421}}
                                       #:dustingetz{:gender {(shirt-size dustingetz/gender) 17592186045421}}],
      (shirt-size :dustingetz/female) 17592186045425,
      (shirt-size :dustingetz/male)   17592186045421}

 (partitions
  (trace
   (hf-pull {(submissionS needle) [{:dustingetz/gender [{(shirt-size dustingetz/gender) [:db/id]}]}]} nil {'needle "example"})))
 := '{(submissionS "example")         [#:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:id 17592186045425}}}
                                       #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:id 17592186045421}}}
                                       #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:id 17592186045421}}}],
      (shirt-size :dustingetz/female) #:db{:id 17592186045425},
      (shirt-size :dustingetz/male)   #:db{:id 17592186045421}}

 (partitions
  (trace
   (hf-pull
    {(submissionS needle)
     [{:dustingetz/gender
       [{(shirt-size dustingetz/gender)
         [:db/ident]}]}]}
    nil
    {'needle "example"})))

 := '{(submissionS "example")         [#:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/womens-medium}}}
                                       #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/mens-small}}}
                                       #:dustingetz{:gender {(shirt-size dustingetz/gender) #:db{:ident :dustingetz/mens-small}}}],
      (shirt-size :dustingetz/female) #:db{:ident :dustingetz/womens-medium},
      (shirt-size :dustingetz/male)   #:db{:ident :dustingetz/mens-small}}


 (partitions
  (trace
   (hf-pull [{(submissionS needle) [{:dustingetz/gender [:db/ident
                                                         {(shirt-sizes dustingetz/gender) [:db/ident]}]}]}
             {(genders) [:db/ident]}]
            nil
            {'needle "example"})))
 :=
 '{(submissionS "example")          [#:dustingetz{:gender {:db/ident                       :dustingetz/female,
                                                           (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/womens-medium}
                                                                                            #:db{:ident :dustingetz/womens-large}
                                                                                            #:db{:ident :dustingetz/womens-small}]}}
                                     #:dustingetz{:gender {:db/ident                       :dustingetz/male,
                                                           (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                                            #:db{:ident :dustingetz/mens-medium}
                                                                                            #:db{:ident :dustingetz/mens-large}]}}
                                     #:dustingetz{:gender {:db/ident                       :dustingetz/male,
                                                           (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                                            #:db{:ident :dustingetz/mens-medium}
                                                                                            #:db{:ident :dustingetz/mens-large}]}}],
   (shirt-sizes :dustingetz/female) [#:db{:ident :dustingetz/womens-medium}
                                     #:db{:ident :dustingetz/womens-large}
                                     #:db{:ident :dustingetz/womens-small}],
   (shirt-sizes :dustingetz/male)   [#:db{:ident :dustingetz/mens-small}
                                     #:db{:ident :dustingetz/mens-medium}
                                     #:db{:ident :dustingetz/mens-large}],
   (genders)                        [#:db{:ident :dustingetz/male}
                                     #:db{:ident :dustingetz/female}]}

 (hf-pull [{:db/ident [:db/id]}] 17592186045418)
 := #:db{:ident #:db{:id 17592186045418}}

 (hf-pull [{:dustingetz/gender [{:db/ident [:db/id]}]}] 17592186045421)
 := #:dustingetz{:gender #:db{:ident #:db{:id 17592186045418}}}

 (hf-pull [{:dustingetz/gender [{:db/ident [{:db/ident [:db/ident]}]}]}] 17592186045421)
 := #:dustingetz{:gender #:db{:ident #:db{:ident #:db{:ident :dustingetz/male}}}}

 (hf-pull [{:dustingetz/gender [:db/id]}] 17592186045421)
 := #:dustingetz{:gender #:db{:id 17592186045418}}

 (hf-pull [{:dustingetz/gender [{:db/id [:db/id]}]}] 17592186045421)
 := #:dustingetz{:gender #:db{:id #:db{:id 17592186045418}}}


 (hf-pull [{:dustingetz/gender [{:db/id [{:db/id [{:db/id [:db/id]}]}]}]}] 17592186045421)
 := #:dustingetz{:gender #:db{:id #:db{:id #:db{:id #:db{:id 17592186045418}}}}}
 )
