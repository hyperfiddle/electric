(ns leo.dataflow
  (:refer-clojure :exclude [compile extend])
  (:require [clojure.tools.analyzer.jvm :as clj]
            [cljs.analyzer.api :as cljs]
            [minitest :refer [tests]]
            [missionary.core :as m]
            [dustin.fiddle :as f])
  (:import (clojure.lang Compiler$LocalBinding)))

;; RUNTIME
(defn extend [flow] (throw (ex-info "Can't call extend outside of dataflow." {:flow flow})))

(defn if! [test then else]
  (m/signal! (m/relieve {} (m/ap (m/?! (if (m/?! test) then else))))))

(defn join! [target]
  (m/signal! (m/relieve {} (m/ap (m/?! (m/?! target))))))

(def input! (comp m/signal! m/latest constantly))

(def apply! (comp m/signal! (partial m/latest (fn [f & args] (apply f args)))))

;; COMPILATION
(def specials `{deref :join extend :extend if :if unquote :unquote})

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
  (let [special? (fn [ast] (and (= :var (:op ast)) (specials (symbol (:var ast)))))
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
          (if-some [special (special? (:fn ast))]
            {:type special
             :deps (map (partial walk env) (:args ast))}
            {:type :apply
             :deps (map (partial walk env) (cons (:fn ast) (:args ast)))})


          (:do)
          (walk env (:ret ast))                       ;; TODO track statements

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
                  :join (cons `join! (map sym (:deps node)))
                  :extend (cons `input! (map sym (:deps node))))))))
        (list `let)
        (list `m/reactor)))))

(defn compile [main env prefix]
  (->> main
    (analyze-clj env)
    (normalize-ast)
    (emit prefix)))

(defmacro dataflow [main]
  (compile main &env (gensym "df")))

(defn dataflow! [node]
  ((fn walk [sort node]
     (if (contains? sort node)
       sort (let [sort (reduce walk sort (:deps node))]
              (assoc sort node
                          (case (:type node)
                            :input (input! (:form node))
                            :apply (apply! (map sort (:deps node)))))))) {} node))


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
       (let [i @(m/watch !input)]
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
      @(map-task #(m/via m/cpu (d/q %)) >needle)))

  (defn lift-task [f]
    (fn [>x] (map-task f >x)))

  (def db (lift-task db-query))

  (defn input-text-values [id]
    (m/observe
      (fn [!] (.addListener (.getElementById js/document id) !)
        #(.removeListener (.getElementById js/document id) !))))

  (deflow async [f x]
    @(map-task f (extend x)))

  (deflow db-query2 [x]
    (async db-query x))

  (macroexpand
    '(dataflow
       (let [i @(input-text-values)]
         (db-query (+ i i)))

       )
    )

  (deflow effect-sync [f x]
    @(m/latest f (extend x)))

  (deflow effect-async [f x]
    (let [>x (extend x)]
      @(m/ap (m/? (let [x (m/?! >x)]
                    (m/via m/blk (f x)))))))

  (deflow effect-async [f x]
    @(map-task #(m/via m/blk (f %)) (extend x)))

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
      (let [[needle] @>route]
        (effect-async (partial swap! effects conj)
          (vector (render-table @(map-task query (extend needle)))
            (if @>open
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

(defn trace-input! [symbol trace-in]
  (->> trace-in
    (m/transform (comp
                   (map #(get % symbol ::unchanged))
                   (remove #{::unchanged})
                   (dedupe)))
    (m/relieve {})
    (m/signal!)))

(defn trace-output! [nodes]
  (m/stream! (m/relieve merge (m/ap (let [[k v] (m/?= (m/enumerate nodes))]
                                      {k (m/?? v)})))))

(defn foreach! [f i]
  (m/stream! (m/ap (m/? (f (m/?? i))))))

(defn debug [nodes port]
  (into [] (map (fn [node]
                  (comment TODO)
                  node)) nodes))

(defn trace
  ([dag inputs] (trace dag inputs resolve))
  ([dag inputs resolve]
   (m/reactor
     (let [trace-in (m/stream! (m/ap (m/? (m/?? (m/enumerate (repeat (m/?= (m/enumerate inputs))))))))]
       (->> (keys dag)
         (reduce
           (fn walk [nodes node]
             (if (contains? nodes node)
               nodes (cond
                       (seq? node)
                       (if-some [s (specials (first node))]
                         (let [nodes (reduce walk nodes (next node))]
                           (->> (next node)
                             (map nodes)
                             (apply (case s
                                      :join join!
                                      :extends input!
                                      :if if!))
                             (assoc nodes node)))
                         (let [nodes (reduce walk nodes node)]
                           (->> node
                             (map nodes)
                             (apply apply!)
                             (assoc nodes node))))

                       (symbol? node)
                       (assoc nodes node (if (namespace node)
                                           (input! (doto (resolve node)
                                                     (when-not (throw (ex-info "Resolution failure." {:symbol node})))))
                                           (trace-input! node trace-in)))

                       ()
                       (assoc nodes node (input! node)))))
           {})
         (reduce-kv (fn [out->nodes node sig]
                      (reduce-kv (fn [out->nodes out sym]
                                   (update out->nodes out assoc sym sig))
                        out->nodes (dag node))) {})
         (run! (fn [[out sym->sig]] (foreach! out (trace-output! sym->sig)))))))))

(comment
  (def client-app
    (dataflow
      (let [[needle] @>route]
        (effect-async (partial swap! effects conj)
          (vector (render-table (query-pink needle))
            (if @>open
              (render-table (query-purple "alice"))
              ::nothing))))))

  ;; program requested by client to server

  (defn query [x]
    (prn :query x) x)

  (def in (m/mbx))
  (defn out-pink [x] (m/via m/blk (prn :out x)))
  (defn out-purple [x] (m/via m/blk (prn :out x)))

  (def server-pink
    (-> {`(query ~'needle) {out-pink 'query-needle}}
      (trace #{in} {`query query})))

  (def server-purple
    (-> {`(query "alice") {out-purple 'query-alice}}
      (trace #{in} {`query query})))

  (def c (server prn prn))

  (in '{needle "charlie"})

  (def rdv (m/rdv))
  (m/? (m/join vector (rdv 3) rdv)) := [nil 3]

  )

(comment
  (let [[needle] @>route]
    (effect-async (partial swap! effects conj)
      (vector (render-table (query needle))
        (if @>open
          (render-table (query "alice"))
          ::nothing)))))

(comment
  (vector (render-table (query (first @>route)))
    (if @>open
      (render-table (query "alice"))
      ::nothing)))

(comment

  (def client->server (m/rdv))
  (def server->client (m/rdv))

  (def !route (atom ["charlie"]))
  (def >route (m/watch !route))

  (def !open (atom false))
  (def >open (m/watch !open))

  (defn log [x] (m/sp (prn x)))

  (defn render-table [query-result]
    [:table (map (partial vector :tr) query-result)])

  (defn query [x] [x])

  (def client-reactor
    (-> {`(first @>route) {client->server 'needle}
         `(vector
            (render-table ~'query-needle)
            (if @>open
              (render-table ~'query-alice)
              ::nothing)) {log 'view}}
      (trace #{server->client})))
  (def cancel-client (client-reactor prn prn))

  (def server-reactor
    (-> {`(query ~'needle) {server->client 'query-needle}
         `(query "alice") {server->client 'query-alice}}
      (trace #{client->server})))
  (def cancel-server (server-reactor prn prn))

  (swap! !open not)
  (reset! !route ["bob"])
  )