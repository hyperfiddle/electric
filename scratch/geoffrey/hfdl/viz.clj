(ns geoffrey.hfdl.viz
  (:require [dorothy.core :as dot]
            [dorothy.jvm :as djvm]
            [minitest :refer [tests]]
            [leo.dataflow :as dataflow]
            [clojure.string :as str]))

(def analyse dataflow/analyze-clj)
(def normalize dataflow/normalize-ast)

(def gen-name (comp str hash))

(defn clean-fn [[f args body]]
  (case (first body)
    . (clean-fn (list f args (rest body)))
    (if (= (count (rest body)) (count args))
      (first body)
      body)))

;; TODO use meander
(defn clean-form [form]
  (if (seq? form)
    (case (first form)
      clojure.core/fn (clean-fn form)
      .               (clean-form (rest form))
      (first form))
    form))

(defn join? [{:keys [type deps] :as ast}]
  (and (= :apply type)
       (= `clojure.core/deref (-> deps first :form))))

(defn leaf? [ast]
  (empty? (:deps ast)))

(defn has-parent? [r]
  (ffirst r))

(defn node [nom props r]
  (cons [nom props] r))

(defn parent-name [r props]
  (let [nom (ffirst r)]
    (if-let [port-pos (:port-pos props)]
      (str nom port-pos)
      nom)))

(defn backlink [nom props r r']
  (cons [nom (parent-name r props) props] r'))

(defn emit
  ([ast] (cons [:node {:shape "none"}]
               (emit (list ["out" {:label ""}]) {} ast)))
  ([r props {:keys [type deps form] :as ast}]
   (let [nom (gen-name ast)]
     (cond
       (leaf? ast)     (cond->> (node nom {:label (str "<b>" (clean-form form) "</b>")} r)
                         (has-parent? r) (backlink nom props r))
       (join? ast)     (emit r {:label " join" :penwidth 2} (second deps))
       (= :if type)    (let [r' (node nom {:label "", :shape "diamond"} r)]
                         (cond->> (mapcat  (fn [label port-pos ast]
                                             (emit r' {:headlabel (str label)
                                                       :port-pos  port-pos}
                                                   ast))
                                           [:test #{true} #{nil false}]
                                           [:w :n :e]
                                           deps)
                           ;; link to parent, if it exists
                           (has-parent? r) (backlink nom props r)))
       (= :apply type) (let [r' (node nom {:label "apply"} r)]  ; make current node
                         (cond->> (mapcat (fn [idx ast] ; recur on each child branch
                                            (emit r' {:headlabel idx} ast))
                                          (range (count deps))
                                          deps)
                           ;; link to parent, if it exists
                           (has-parent? r) (backlink nom props r)))))))

(def emit-dot (comp emit normalize analyse))

(def viz! (comp djvm/show! dot/dot))

(comment
  (declare input0 input1 input2)
  (viz! [[:a :b]])
  (viz! (emit-dot {} '(+ (inc 0) (dec 0))))
  (viz! (emit-dot {} '@input1))
  (viz! (emit-dot {} '@(inc input1)))
  (viz! (emit-dot {} '(+ @(missionary.core/watch input1)
                         @(missionary.core/watch input2))))
  (viz! (emit-dot {} '@(if (odd? @(missionary.core/watch input0))
                         (missionary.core/watch input1)
                         (missionary.core/watch input2)))))

(tests
 ;; Simple let
 (emit-dot {}
           '(let [z 0
                  a (inc z)
                  b (dec z)]
     (+ a b)))
 := [[:node {:shape "none"}]
     [:z0 {:label "0\n#{z}"}]
     [:a0 {:label "(inc arg0)\n#{a}"}]
     [:b0 {:label "(dec arg0)\n#{b}"}]
     [:z0 :a0]
     [:z0 :b0]
     [:% {:label "(+ arg1 arg2)"}]
     [:a0 :% {:headlabel "1"}]
     [:b0 :% {:headlabel "2"}]])


(tests
 ;; Arrow
 (emit-dot {}
  '(+ @(m/watch input1)
      @(m/watch input2)))
 := [[:node {:shape "none"}]
     [:branch_0 {:label "(m/watch input1)"}]
     [:branch_1 {:label "(m/watch input2)"}]
     [:out {:label "(+ %1 %2)"}]
     [:branch_0 :out {:label "join", :headlabel "1"}]
     [:branch_1 :out {:label "join", :headlabel "2"}]])

(tests
 ;; Conditionals
 (emit-dot {}
  '@(if (odd? @(m/watch input))
      (m/watch branchA)
      (m/watch branchB)))
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
