(ns leo.dataflow
  (:refer-clojure :exclude [compile])
  (:require [clojure.tools.analyzer.jvm :as clj]
            [cljs.analyzer.api :as cljs]
            [minitest :refer [tests]]
            [missionary.core :as m]
            [dustin.fiddle :as f])
  (:import (clojure.lang Compiler$LocalBinding)))

;; RUNTIME
(defn <- [flow] (throw (ex-info "Can't call <- outside of dataflow." {:flow flow})))

(defn if! [test then else]
  (m/signal! (m/relieve {} (m/ap (m/?! (if (m/?! test) then else))))))

(defn join! [target]
  (m/signal! (m/relieve {} (m/ap (m/?! (m/?! target))))))

(def input! (comp m/signal! m/latest constantly))

(def apply! (comp m/signal! (partial m/latest (fn [f & args] (apply f args)))))

;; COMPILATION
(def analyze-clj
  (let [scope-bindings
        (partial reduce-kv
          (fn [scope symbol binding]
            (assoc scope
              symbol (or (when (instance? Compiler$LocalBinding binding)
                           (let [binding ^Compiler$LocalBinding binding]
                             {:op   :local
                              :tag  (when (.hasJavaClass binding)
                                      (some-> binding (.getJavaClass)))
                              :form symbol
                              :name symbol}))
                       binding))) {})]
    (fn [env form]
      (if (:js-globals env)
        (cljs/analyze env form)
        (binding [clj/run-passes clj/scheduled-default-passes]
          (->> env
            (scope-bindings)
            (update (clj/empty-env) :locals merge)
            (clj/analyze form)))))))

(def normalize-ast
  (let [join? (every-pred (comp #{:var} :op) (comp #{`<-} symbol :var))
        syms (fn [p n] (into [] (map (comp symbol (partial str p))) (range n)))]
    (partial
      (fn walk [env {:keys [op] :as ast}]
        (case op
          (:const)
          {:type :input
           :form (:val ast)}

          (:var)
          {:type :input
           :form (symbol (:var ast))}

          (:static-call)
          (let [{:keys [class method args]} ast
                arg-syms (syms "arg" (count args))]
            {:type :apply
             :deps (cons
                     {:type :input
                      :form (->> arg-syms
                              (cons (symbol (.getName ^Class class) (name method)))
                              (cons `.)
                              (list `fn arg-syms))}
                     (map (partial walk env) (:args ast)))})

          (:invoke)
          (if (join? (:fn ast))
            {:type :join
             :deps (map (partial walk env) (:args ast))}
            {:type :apply
             :deps (map (partial walk env) (cons (:fn ast) (:args ast)))})

          (:do)
          (walk env (:ret ast))                       ;; fantasyland, discard statements

          (:let)
          (walk (reduce (fn [env {:keys [name init]}]
                          (assoc env name (walk env init)))
                  env (:bindings ast)) (:body ast))

          (:local)
          (or (env (:name ast))
            {:type :input
             :form (:name ast)})

          (:if)
          {:type :if
           :deps [(walk env (:test ast))
                  (walk env (:then ast))
                  (walk env (:else ast))]}

          )) {})))

(defn topsort [deps node]
  ((fn walk [sort node]
     (if (contains? sort node)
       sort (let [sort (reduce walk sort (deps node))]
              (assoc sort node (count sort))))) {} node))

(tests
  (topsort next [:a [:b [:d]] [:c [:d]]]) :=
  {[:d] 0,
   [:b [:d]] 1,
   [:c [:d]] 2,
   [:a [:b [:d]] [:c [:d]]] 3})

(def emit
  (fn [prefix graph]
    (let [ids (topsort :deps graph)
          sym (comp symbol (partial str prefix "-") ids)]
      (->> (keys ids)
        (sort-by ids)
        (into []
          (mapcat
            (juxt sym
              (fn [node]
                (case (:type node)
                  :input (list `input! (:form node))
                  :apply (cons `apply! (map sym (:deps node)))
                  :if (cons `if! (map sym (:deps node)))
                  :join (cons `join! (map sym (:deps node))))))))
        (list `let)))))

(defn compile [main env prefix]
  (->> main
    (analyze-clj env)
    (normalize-ast)
    (emit prefix)))

(defmacro dataflow [main]
  (compile main &env (gensym "df")))

(comment
  (def !input (atom 0))
  (def !input2 (atom 0))

  (def !output (atom 0))

  (macroexpand '(dataflow (+ 1 2)))
  (macroexpand '(dataflow (let [a 1] (+ a a))))
  (macroexpand '(dataflow (let [i (m/watch !input)]
                            (if (odd? i) (inc i) i))))

  (macroexpand
    '(dataflow
       (let [i (<- (m/watch !input))]
         (vector
           (if (odd? i) (inc i) i)
           (* (m/watch !input2) 2)))))

  (def input
    (->>
      (m/ap (doto (m/? (m/sleep 1000 (m/?? (m/enumerate (range 10))))) prn))
      (m/relieve {})))

  ;; f: x -> task[y], cont flow
  (defn map-task [f >x]
    (m/ap (m/? (f (m/?! >x)))))

  (deflow db-query [needle]
    (let [>needle (extend needle)]
      (<- (map-task #(m/via m/cpu (d/q %)) >needle))))

  (defn lift-task [f]
    (fn [>x] (map-task f >x)))

  (def db (lift-task db-query))

  (defn input-text-values [id]
    (m/observe
      (fn [!] (.addListener (.getElementById js/document id) !)
        #(.removeListener (.getElementById js/document id) !))))

  (deflow async [f x]
    (<- (map-task f (extend x))))

  (deflow db-query2 [x]
    (async db-query x))

  (macroexpand
    '(dataflow
       (let [i (<- (input-text-values))]
         (db-query (+ i i)))

       )
    )

  (deflow effect-sync [f x]
    (<- (m/latest f (extend x))))

  (deflow effect-async [f x]
    (let [>x (extend x)]
      (<-
        (m/ap (m/? (let [x (m/?! >x)]
                     (m/via m/blk (f x))))))))

  (deflow effect-async [f x]
    (<- (map-task #(m/via m/blk (f %)) (extend x))))

  (defn render-table [query-result]
    (prn :render-table query-result)
    [:table (map (partial vector :tr) query-result)])

  (defn query [needle]
    (m/via m/cpu (f/submissions needle)))

  (def !route (atom ["alice"]))
  (def >route (m/watch !route))
  (def !open (atom false))
  (def >open (m/watch >open))
  (def effects (atom []))

  (def app
    (dataflow
      (let [[needle] (<- >route)]
        (effect-async (partial swap! effects conj)
          (vector (render-table (<- (map-task query (extend needle))))
            (if (<- >open)
              (render-table (map-task query (extend "alice")))
              ::nothing))))))
  (def cancel (app prn prn))
  (reset! >route ["bob"])
  (reset! >open true)
  @effects :=
  [[[:table [:tr 9]] ::nothing]
   [[:table [:tr 10]] ::nothing]
   [[:table [:tr 10]] [:table [:tr 9]]]]

  )

















(comment


  (defn fib! [x]
    (m/!)
    (cond
      (= x 0) 1
      (= x 1) 1
      () (+ (fib! (dec x))
           (fib! (dec (dec x))))))

  (defn fib [x] (m/via m/blk (fib! x)))

  (defn stream-fib [>x]
    (->> (m/ap (m/? (fib (m/?! >x))))
      (m/relieve {})))

  (macroexpand
    '(dataflow
       (-> (m/watch !input)
         (<|) (inc)
         (|>) (stream-fib) (<|)

         )


       )

    )


  )