(ns hfdl.viz
  (:require [clojure.string :as str]
            [dorothy.core :as dot]
            [dorothy.jvm :as djvm]
            [hfdl.lang :refer [dataflow]]))

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

(defn record "Render a collection to a record node"
  ([type coll] (record type nil coll))
  ([type label coll]
   (let [[open close] (delimiters type)]
     (cond-> ""
       (some? label) (str label "|")
       true          (str open (-> (apply str' coll)
                                   (str/replace #"\{" "\\\\{")
                                   (str/replace #"\}" "\\\\}"))
                          close)))))

(defn node-type
  ([node]       (first node))
  ([program ip] (node-type (get program ip))))

(defn label [idx [type & args]]
  (case type
    (:local :global)             (str' (first args))
    (:constant :variable :apply) (pretty-arg idx)))

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

(defn render-hash-map [visited? program ip]
  (let [[_ _ args :as instr] (get program ip)]
    (concat
     ;; child nodes
     (mapcat (fn [child-ip] (when-not (value? program child-ip) (render-node visited? program child-ip))) args)
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
                      [child-ip ip (assoc default-link-props :headport (str ip "_" idx (if (even? idx) ":w" ":e")))]))
                  args))))

(defn render-form [visited? program ip]
  (let [[_ f-ip args-ip :as instr] (get program ip)
        [_ f-sym]                  (get program f-ip)
        f-sym                      (if (collection? f-sym) f-sym :sexp)
        args-ip                    (if (collection? f-sym) args-ip (into [f-ip] args-ip))]
    (concat
     ;; child nodes
     (mapcat (fn [arg-ip]
               (when-not (value? program arg-ip)
                 (render-node visited? program arg-ip)))
             args-ip)
     ;; current node
     [[ip (cond-> {:label    (->> (map-indexed (fn [child-idx child-ip]
                                                 (render-cell program ip child-idx child-ip))
                                               args-ip)
                                  (intersperse "|")
                                  (record f-sym (pretty-arg ip)))
                   :shape    :record
                   :penwidth 0}
            (:source (meta instr)) (assoc :cluster (:source (meta instr))))]]
     ;; links
     (map-indexed (fn [child-idx child-ip]
                    (when-not (value? program child-ip)
                      [child-ip ip (assoc default-link-props :headport (str ip "_" child-idx ":n"))]))
                  args-ip))))

(defn render-apply-node [visited? program ip]
  (let [[_ f-ip _] (get program ip)
        [_ f-sym]  (get program f-ip)]
    (case f-sym
      clojure.core/hash-map (render-hash-map visited? program ip)
      (render-form visited? program ip))))


(defn render-node [visited? program ip]
  (when-not (@visited? ip)
    (swap! visited? conj ip)
    (let [[type & args :as instr] (get program ip)]
      (case type
        (:local :global)      [[ip {:label (str (pretty-arg ip) " " (label ip instr)), :shape :none}]]
        (:variable :constant) (let [[child-ip] args]
                                (concat [[ip (cond-> {:shape (case type :variable :invtriangle, :constant :triangle)
                                                      :label (pretty-arg ip)}
                                               (:source (meta instr)) (assoc :cluster (:source (meta instr))))]]
                                        (render-node visited? program child-ip)
                                        [[child-ip ip {:concentrate :true, :arrowtail :odot}]]))
        :apply                (render-apply-node visited? program ip)))))

(defn node? [node] (= 2 (count node)))
(defn props [[_id props]] props)
(defn cluster [node] (:cluster (props node)))

(defn clusters [nodes]
  (->> (filter node? nodes)
       (filter cluster)
       (group-by cluster)
       (map (fn [[cluster-id nodes]]
              (dot/subgraph (str "cluster_" cluster-id)
                            (cons {:label cluster-id}
                                  (map (fn [[id _]] [id])
                                       nodes)))))))

(defn dot "Transform an HFIL `program` into dot representation"
  [program]
  (let [graph (filter identity (render-node (atom #{}) program (dec (count program))))]
    (concat (clusters graph) graph)))

;;;;;;;;;;;;;;;;;;;;;;;
;; VISUAL TESTS ZONE ;;
;;;;;;;;;;;;;;;;;;;;;;;

(do
  (defmacro ^:private hfil [& body] ; for convenience
    `(:graph (dataflow ~@body)))

  (defn compile [program]
    (->> program
         (dot)
         (cons [:edge {:arrowhead :none}])
         (dot/digraph)
         (dot/dot)))

  (defn viz! [program]
    (djvm/show! (compile program)))

  (defn save! [file program]
    (djvm/save! (compile program) file {:format :png}))


  (comment
    (declare a b c n)

    (viz! (hfil (if (odd? 1) :a :b)))

    (viz! (hfil (case 1
                  (1 3) :odd
                  2     :even
                  4     :even
                  :default)))

    (viz! (hfil {:data-structures {:static  {:map    {:foo :bar}
                                             :set    #{:a :b :c}
                                             :vector [:a :b :c]
                                             :list   '(:a :b :c)}
                                   :dynamic {:map    {:a @b
                                                      @a @b}
                                             :set    #{@a :b :c :d}
                                             :vector [@a [:b] :c :d]
                                             :list   (list :a @b :c)}}}))

    (save! "/tmp/fizzbuz.png"
          (hfil (let [n @n]
                  (cond
		                (zero? (mod n 15)) "FizzBuzz"
		                (zero? (mod n 3)) "Fizz"
		                (zero? (mod n 5)) "Buzz"
		                :else n))))

    ))
