(ns geoffrey.hfdl.viz
  (:require [dorothy.core :as dot]
            [dorothy.jvm :as djvm]
            [minitest :refer [tests]]
            [leo.dataflow :as dataflow]
            [clojure.string :as str]))

(def analyse dataflow/analyze-clj)
(def normalize dataflow/normalize-ast)

(def gen-name (comp keyword gensym))

;; TODO replace with meander
(defn clean-form [form]
  (if (seq? form)
    (case (first form)
      clojure.core/fn (clean-form (nth form 2))
      .               (clean-form (rest form))
      (cond
        (str/starts-with? (namespace (first form)) "clojure.lang")
        ,, (cons (symbol (name (first form))) (rest form))

        :else form))
    form))

(defn emit
  ([ast] (cons [:node {:shape "none"}]
               (emit () (atom {}) ast)))
  ([r !registry {:keys [type deps form] :as ast}]
   (case type
     ;; We jump over apply nodes
     :apply (let [r' (emit r !registry (first deps))]
              (mapcat #(emit r' !registry %) (rest deps)))
     ;; else
     (if (empty? deps) ; leaf
       (let [name (or (get @!registry form) (gen-name))]
         (swap! !registry assoc form name) ; unify nodes by form, for RT
         (->> r
              (cons [name {:label (str (clean-form form))}]) ; leaf node
              (cons [name (ffirst r)])))                     ; link to parent
       (let [name (gen-name)
             r'   (cons [name {:label (str (clean-form form))}] r)]
         (->> (mapcat #(emit r' !registry %) deps)
              (cons [name (ffirst r)]) ; link to parent
              ))))))

(def emit-dot (comp emit normalize analyse))

(def viz! (comp djvm/show! dot/dot))

(comment
  (viz! [[:a :b]])
  (viz! (emit-dot {} '(+ (inc 0) (dec 0)))))

(tests
 ;; Simple let
 (emit-dot {}
           '(let [z 0
                  a (inc z)
                  b (dec z)]
     (+ a b)))
 := (viz! [[:node {:shape "none"}]
          [:z0 {:label "0\n#{z}"}]
          [:a0 {:label "(inc arg0)\n#{a}"}]
          [:b0 {:label "(dec arg0)\n#{b}"}]
          [:z0 :a0]
          [:z0 :b0]
          [:% {:label "(+ arg1 arg2)"}]
          [:a0 :% {:headlabel "1"}]
          [:b0 :% {:headlabel "2"}]]))


(tests
 ;; Arrow
 (emit-dot {}
  '(+ (<- (m/watch input1))
      (<- (m/watch input2))))
 := [[:node {:shape "none"}]
     [:branch_0 {:label "(m/watch input1)"}]
     [:branch_1 {:label "(m/watch input2)"}]
     [:out {:label "(+ %1 %2)"}]
     [:branch_0 :out {:label "join", :headlabel "1"}]
     [:branch_1 :out {:label "join", :headlabel "2"}]])

(tests
 ;; Conditionals
 (emit-dot {}
  '(<- (if (odd? (<- (m/watch input)))
         (m/watch branchA)
         (m/watch branchB))))
 := [[:node {:shape "none"}]
     [:branch_0 {:label "(m/watch input)"}]
     [:branch_1 {:label "(odd? %)"}]
     [:branch_0 :branch_1 {:label "join"}]
     [:branch_2 {:label "(m/watch branchA)"}]
     [:branch_3 {:label "(m/watch branchB)"}]
     [:branch_4 {:label "", :shape "diamond"}]
     [:branch_1 :branch_4 {:headlabel ":test"}]
     [:branch_2 :branch_4 {:headlabel "#{true}"}]
     [:branch_3 :branch_4 {:headlabel ":default"}]
     [:branch_5 {:label ""}]
     [:branch_4 :branch_5 {:label "join"}]])
