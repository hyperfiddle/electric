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
  ([node]             (first node))
  ([program instr-id] (node-type (get program instr-id))))

(defn label [idx [type & args]]
  (case type
    (:local :global)             (str' (first args))
    (:constant :variable :apply) (pretty-arg idx)))

(def collection? `#{vector list hash-set hash-map})

(defn intersperse
  "Like `interpose`, but also prepend and append `sep` in the result."
  [sep coll]
  (concat (interleave (repeat sep) coll) (list sep)))

(defn render-cell [program instr-id child-idx child-id]
  (str "<" instr-id "_" child-idx "> " (label child-id (get program child-id))))

(defn render-hash-map-keys [program instr-id args]
  (map (fn [child-idx child-id] (render-cell program instr-id (inc child-idx) child-id))
       (range 0 (count args) 2)
       (take-nth 2 (next args))))

(defn render-hash-map-vals [program instr-id args]
  (map (fn [child-idx child-id] (render-cell program instr-id (inc child-idx) child-id))
       (range 1 (count args) 2)
       (take-nth 2 (nnext args))) )

(defn render-hash-map [program instr-id args]
  [instr-id {:label    (str (pretty-arg instr-id)
                            "|"
                            (record `hash-map (interpose "|" (render-hash-map-keys program instr-id args)))
                            "|"
                            (record `hash-map (interpose "|" (render-hash-map-vals program instr-id args))))
             :shape    :record
             :penwidth 0.2}])

(defn apply-node [program instr-id [function-id function-args-ids]]
  (let [[_ function-sym] (get program function-id)
        fapply-args      (into [function-id] function-args-ids)
        links            (->> (map (fn [child-idx parent-id child-id]
                                     (when-not (#{:local :global} (node-type program parent-id))
                                       [parent-id child-id {:labelfloat  :true ; move labels so they don't overlap
                                                            :headport    (str instr-id "_" child-idx (if (= `hash-map function-sym)
                                                                                                       (if (odd? child-idx) ":w" ":e") ; key or val
                                                                                                       ":n"))
                                                            :tailport    "s"
                                                            :concentrate :true
                                                            :arrowhead   :odot}]))
                                   (range (count fapply-args)) ; to mimic map-indexed
                                   fapply-args
                                   (repeat (count fapply-args) instr-id) ; fixed child-id
                                   )
                              (filter identity))
        node             (if (= `hash-map function-sym)
                           (render-hash-map program instr-id fapply-args)
                           (let [coll?       (collection? function-sym)
                                 record-type (if coll? function-sym :sexp)
                                 fapply-args (map-indexed (fn [child-idx parent-id]
                                                            (render-cell program instr-id (if coll? (inc child-idx) child-idx) parent-id))
                                                          (if coll? (next fapply-args) fapply-args))]
                             [instr-id {:label    (record record-type (pretty-arg instr-id) (intersperse "|" fapply-args))
                                        :shape    :record
                                        :penwidth 0}]))]
    (into [node] links)))

(defn instruction->node
  "Transform an HFIL instruction at `instr-id` in HFIL `program` into a set
  of dot nodes and links."
  [program instr-id]
  (let [[type & args] (get program instr-id)]
    (case type
      :apply                (apply-node program instr-id args)
      (:variable :constant) (let [[parent-id] args
                                  node        [instr-id {:shape (case type :variable :invtriangle, :constant :triangle)
                                                         :label (pretty-arg instr-id)}]
                                  link        [parent-id instr-id {:concentrate :true, :arrowtail :odot}]]
                              (cond-> [node link]
                                (#{:local :global} (node-type program parent-id))
                                ,, (conj [parent-id {:label  (str (pretty-arg parent-id) " " (label parent-id (get program parent-id)))
                                                     :shape  :none}])))
      (:local :global)      [] ; skip. Will be rendered by dependents.
      )))

(defn dot "Transform an HFIL `program` into dot representation"
  [program]
  (->> (range (count program))
       (map #(instruction->node program %))
       (mapcat identity)
       (into [])))

(do
;;;;;;;;;;;;;;;;;;;;;;;
;; VISUAL TESTS ZONE ;;
;;;;;;;;;;;;;;;;;;;;;;;

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

    (viz! #_"/tmp/fizzbuz.png"
          (hfil (let [n @n]
                  (cond
		                (zero? (mod n 15)) "FizzBuzz"
		                (zero? (mod n 3)) "Fizz"
		                (zero? (mod n 5)) "Buzz"
		                :else n))))

    ))
