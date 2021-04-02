(ns hfdl.viz
  (:require [clojure.string :as str]
            [dorothy.core :as dot]
            [dorothy.jvm :as djvm]
            [hfdl.lang :refer [dataflow debug! heap-dump]]
            [missionary.core :as m]
            [minitest :refer [tests]]))

(def pretty-syms '[⓪ ① ② ③ ④ ⑤ ⑥ ⑦ ⑧ ⑨ ⑩ ⑪ ⑫ ⑬ ⑭ ⑮ ⑯ ⑰ ⑱ ⑲ ⑳ ㉑ ㉒ ㉓ ㉔ ㉕ ㉖ ㉗ ㉘ ㉙ ㉚ ㉛ ㉜ ㉝ ㉞ ㉟ ㊱ ㊲ ㊳ ㊴ ㊵ ㊶ ㊷ ㊸ ㊹ ㊺ ㊻ ㊼ ㊽ ㊾ ㊿])

(defn pretty-arg
  "Return a UTF-8 Circled Number representation of `n`, or `n` itself if not
  found."
  [n]
  (str (if (<= 0 n (dec (count pretty-syms)))
         (get pretty-syms n)
         n)))

(defn str'
  "Like `str` but preserve nil."
  [& args]
  (apply str (replace {nil "nil"} args)))

(def delimiters `{vector   ["\\[" "\\]"]
                  hash-set ["#\\{" "\\}"]
                  list     ["' (" ")"]
                  :sexp    ["(" ")"]
                  hash-map ["{" "}"]})

(defn escape-str [s]
  (-> (str/replace s #"\{" "\\\\{")
      (str/replace #"\}" "\\\\}")))

(defn record "Render a collection to a record node"
  ([type coll] (record type nil coll))
  ([type label coll]
   (let [[open close] (delimiters type)]
     (cond-> ""
       (some? label) (str label "|")
       true          (str open (escape-str (apply str' coll)) close)))))

(defn node-type
  ([node]       (first node))
  ([program ip] (node-type (get program ip))))

;; TODO keep a map of missionary object to ip …
(defn label
  ([idx [type & args]] ; deprecated
   (case type
     (:local :global)             (str' (first args))
     (:constant :variable :apply) (pretty-arg idx)))
  ([program heap ip idx]
   (let [[type & args] (get program ip)
         value         (get-in heap [:heap [ip]])]
     (case type
       (:local :global)   (str' (first args))
       (:variable :apply) (str (pretty-arg idx) " = " value)
       (:constant)        (str (pretty-arg idx) " = Constant( " (pretty-arg (first args)) " )"
                               )))))

(def collection? `#{vector list hash-set hash-map})

(defn intersperse
  "Like `interpose`, but also prepend and append `sep` in the result."
  [sep coll]
  (concat (interleave (repeat sep) coll) (list sep)))

(defn render-cell [program ip child-idx child-ip]
  (str "<" ip "_" child-idx "> " (label child-ip (get program child-ip))))

(defn render-hash-map-keys [program ip]
  (let [[_ _ args] (get program ip)]
    (map (fn [child-idx child-id] (render-cell program ip child-idx child-id))
         (range 0 (count args) 2)
         (take-nth 2 args))))

(defn render-hash-map-vals [program ip]
  (let [[_ _ args] (get program ip)]
    (map (fn [child-idx child-id] (render-cell program ip child-idx child-id))
         (range 1 (count args) 2)
         (take-nth 2 (next args)))) )

(defn value? [program ip]
  (#{:local :global} (node-type program ip)))

(def default-link-props {:tailport  "s"
                         :arrowhead :odot
                         :dir       :forward})
(declare render-node)

(defn render-hash-map [visited? program heap ip]
  (let [[_ _ args :as instr] (get program ip)]
    (concat
     ;; child nodes
     (mapcat (fn [child-ip] (when-not (value? program child-ip) (render-node visited? program heap child-ip))) args)
     ;; current node
     [[ip (cond-> {:label    (str (pretty-arg ip)
                                  "|"
                                  (record `hash-map (interpose "|" (render-hash-map-keys program ip)))
                                  "|"
                                  (record `hash-map (interpose "|" (render-hash-map-vals program ip))))
                   :shape    :record
                   :penwidth 0.2}
            (:source (meta instr)) (assoc :cluster (:source (meta instr))))]]
     ;; links
     (map-indexed (fn [idx child-ip]
                    (when-not (value? program child-ip)
                      [child-ip ip (cond-> (assoc default-link-props :headport (str ip "_" idx (if (even? idx) ":w" ":e")))
                                     (contains? (:changes heap) [child-ip]) (assoc :color :red))]))
                  args))))

(defn render-form [visited? program heap ip]
  (let [[_ f-ip args-ip :as instr] (get program ip)
        [_ f-sym]                  (get program f-ip)
        f-sym                      (if (collection? f-sym) f-sym :sexp)
        args-ip                    (if (collection? f-sym) args-ip (into [f-ip] args-ip))
        slot                       (find (:heap heap) [ip])]
    (concat
     ;; child nodes
     (mapcat (fn [arg-ip]
               (when-not (value? program arg-ip)
                 (render-node visited? program heap arg-ip)))
             args-ip)
     ;; current node
     [[ip (cond-> {:label    (cond->   (->> (map-indexed (fn [child-idx child-ip]
                                                       (render-cell program ip child-idx child-ip))
                                                     args-ip)
                                        (intersperse "|")
                                        (record f-sym (pretty-arg ip)))
                               (some? slot) (str  " |= " (escape-str (pr-str (val slot)))))
                   :shape    :record
                   :penwidth 0}
            (:source (meta instr)) (assoc :cluster (:source (meta instr))))]]
     ;; links
     (map-indexed (fn [child-idx child-ip]
                    (when-not (value? program child-ip)
                      [child-ip ip (cond-> (assoc default-link-props :headport (str ip "_" child-idx ":n"))
                                     (contains? (:changes heap) [child-ip]) (assoc :color :red))]))
                  args-ip))))

(defn render-apply-node [visited? program heap ip]
  (let [[_ f-ip _] (get program ip)
        [_ f-sym]  (get program f-ip)]
    (case f-sym
      clojure.core/hash-map (render-hash-map visited? program heap ip)
      (render-form visited? program heap ip))))

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

(defn node? [node] (and (vector? node) (= 2 (count node))))
(defn link? [node] (and (vector? node) (= 3 (count node))))
(defn props [[_id props]] props)

(defn render-prefix [prefix] (str/join "_" prefix))

(defn prefix-nodes [prefix nodes]
  (map (fn [n] (cond
                 (node? n) (-> (assoc n 0 (render-prefix (conj prefix (first n))))
                               (update 1 assoc :cluster (render-prefix prefix)))
                 (link? n) (-> (assoc n 0 (render-prefix (conj prefix (first n))))
                               (assoc 1 (render-prefix (conj prefix (second n)))))))
        nodes))

(defn render-nested-frame [program {:keys [heap changes]} ip]
  (let [[_ child-ip]  (get program ip)
        [type & args] (get program child-ip)]
    (case type
      (:global) (let [program (:graph @(resolve (first args)))
                      last-ip (dec (count program))]
                  (mapcat identity
                          (for [[fork frame] (focus heap [ip])]
                            (let [nodes      (->> (render-node (atom #{})
                                                               program
                                                               {:heap frame :changes changes}
                                                               last-ip)
                                                  (filter identity)
                                                  (prefix-nodes [ip fork]))
                                  cluster    (str "cluster_" (render-prefix [ip fork]))
                                  link-props (cond-> {:arrowhead :odot}
                                               (contains? changes [child-ip]) (assoc :color :red))]
                              (conj nodes
                                    [(render-prefix [ip fork last-ip]) ip (assoc link-props :ltail cluster)]
                                    [child-ip (render-prefix [ip fork 0]) (assoc link-props :lhead cluster)]))))))))

(defn has-nested-frame? [heap ip]
  (->> (keys heap)
       (filter (fn [path] (subpath? [ip] path)))
       (not-empty)))

(defn render-node [visited? program heap ip]
  (when-not (@visited? ip)
    (swap! visited? conj ip)
    (let [[type & args :as instr] (get program ip)]
      (case type
        (:local :global)      [[ip {:label (str (pretty-arg ip) " " (label program heap ip nil)), :shape :none}]]
        (:variable :constant) (let [[child-ip] args]
                                (concat [[ip (cond-> {:shape :none #_(case type :variable :invtriangle, :constant :triangle)
                                                      :label (str (case type :variable "▽ ", :constant "△ ")
                                                                  (label program heap ip ip))}
                                               (:source (meta instr)) (assoc :cluster (:source (meta instr))))]]
                                        (render-node visited? program heap child-ip)
                                        (if (has-nested-frame? (:heap heap) ip)
                                          (render-nested-frame program heap ip)
                                          [[child-ip ip (cond-> {:arrowhead :odot}
                                                          (contains? (:changes heap) [child-ip]) (assoc :color :red))]])))
        :apply                (render-apply-node visited? program heap ip)))))

(defn cluster [node] (:cluster (props node)))

(defn clusters [nodes]
  (->> (filter node? nodes)
       (filter cluster)
       (group-by cluster)
       (map (fn [[cluster-id nodes]]
              (dot/subgraph (str "cluster_" cluster-id)
                            (cons {:label cluster-id
                                   :style :rounded}
                                  (map (fn [[id _]] [id])
                                       nodes)))))))

(defn dot "Transform an HFIL `program` into dot representation"
  ([program] (dot program {} #{}))
  ([program heap] (dot program heap #{}))
  ([program heap changes]
   (let [instructions (:graph program)
         graph        (filter identity (render-node (atom #{})
                                                    instructions
                                                    {:heap    heap
                                                     :changes changes}
                                                    (dec (count instructions))))]
     (concat (clusters graph) graph))))

(defn heap-diff [ma mb]
  (reduce (fn [r [k v]]
            (if (or (not (contains? ma k))
                    (not= (get ma k) v))
              (conj r k)
              r))
          #{}
          mb))

(defn differential-dot [program & [heap]]
  (let [prev (atom heap)]
    (fn [heap]
      (let [prev-heap @prev]
        (reset! prev heap)
        (dot program heap (heap-diff prev-heap heap))))))

;;;;;;;;;;;;;;;;;;;;;;;
;; VISUAL TESTS ZONE ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn compile [dot]
  (->> (cons [:edge {:arrowhead :none, :concentrate :true}] dot)
       (cons {:compound :true})
       (dot/digraph)
       (dot/dot)))

(defn show! [dot] (djvm/show! (compile dot)))
(defn show!* [dot] (djvm/show! dot))

(defn save! [file dot]
  (djvm/save! (compile dot) file {:format :png}))


(comment
  (declare a b c n)
  )

(comment
  (do ; simple deref
    (def !a (atom 0))
    (def a (m/watch !a))
    (def program (dataflow @a))
    (def process (debug! program))
    (def render (differential-dot program))
    (show! (render (heap-dump @process)))
    (swap! !a inc)
    (show! (render (heap-dump @process)))
    )
  (do ; if
    (def !a (atom 1))
    (def a (m/watch !a))
    (def program (dataflow (if (odd? @a) :odd :even)))
    (def process (debug! program))
    (def render (differential-dot program))
    (show! (render (heap-dump @process)))
    (swap! !a inc)
    (show! (render (heap-dump @process)))
    )
  (do ; case
    (def !a (atom 1))
    (def a (m/watch !a))
    (def program (dataflow (case @a
                             (1 3) :odd
                             (2 4) :even
                             :default)))
    (def process (debug! program))
    (def render (differential-dot program))
    (show! (render (heap-dump @process)))
    (swap! !a inc)
    (show! (render (heap-dump @process)))
    )

  (do ; data structures
    (def !a (atom 0))
    (def a (m/watch !a))
    (def !b (atom 0))
    (def b (m/watch !b))

    (def program (dataflow {:data-structures {:static  {:map    {:foo :bar}
                                                        :set    #{:a :b :c}
                                                        :vector [:a :b :c]
                                                        :list   '(:a :b :c)}
                                              :dynamic {:map    {:a @b
                                                                 @a @b}
                                                        :set    #{@a :b :c :d}
                                                        :vector [@a [:b] :c :d]
                                                        :list   (list :a @b :c)}}}))

    (def process (debug! program))
    (def render (differential-dot program))
    (save! "/home/geoffrey/Downloads/ds-pulse-1.png" (render (heap-dump @process)))
    (swap! !a inc)
    (save! "/home/geoffrey/Downloads/ds-pulse-2.png" (render (heap-dump @process)))
    (swap! !b inc)
    (save! "/home/geoffrey/Downloads/ds-pulse-3.png" (render (heap-dump @process)))
    )

  (do ; fizzbuz
    (def !a (atom 0))
    (def a (m/watch !a))
    (def zero? clojure.core/zero?) ; prevent inlining
    (def program (dataflow (let [n @a]
                             (cond
		                           (zero? (mod n 15)) "FizzBuzz"
		                           (zero? (mod n 3))  "Fizz"
		                           (zero? (mod n 5))  "Buzz"
		                           :else              n))))
    (def process (debug! program))
    (def render (differential-dot program))
    (dotimes [x 16]
      (reset! !a x)
      (save! (format "/home/geoffrey/Downloads/fizzbuz/%s.png" x) (render (heap-dump @process)))))

  (do ; nested frames
    (def !input (atom "a"))
    (def >input (m/watch !input))
    (def grandchild (dataflow (str @>input @>input)))
    (def child (dataflow (str @grandchild)))
    (def parent (dataflow [@child]))
    (def p (debug! parent))
    (def render (differential-dot parent))
    (show! (render (heap-dump @p)))
    ;; (reset! !input "b")
    ;; (show! (render (heap-dump @p)))

    )
(do ; nested frame as a function
  (def !a (atom 1)) (def a (m/watch !a))
  (defn program1 [a] (dataflow (inc @a)))
  (def program2 (dataflow @(program1 a)))
  (def process (debug! program2))
  (show! (dot program2 (heap-dump @process))))

  ;; 1. Unhandled java.lang.IllegalArgumentException
  ;; No matching clause: :apply

  ;; viz.clj:  180  hfdl.viz/render-nested-frame


;;;;;;;;;;;;;
;; SCRATCH ;;
;;;;;;;;;;;;;

(comment
  (->> '{[0]     #'hfdl.viz/child,
         [1]     "aa",
         [2]     #'clojure.core/vector,
         [3]     ["aa"],
         [1 0 0] #'hfdl.viz/>input,
         [1 0 1] "a",
         [1 0 2] #'clojure.core/str,
         [1 0 3] "aa"}
        (seq)
        (sort-by first))

  [[:global hfdl.viz/child]
   [:variable 0]
   [:global clojure.core/vector]
   [:apply 2 [1]]]
  ))
