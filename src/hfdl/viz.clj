(ns hfdl.viz
  (:require [clojure.string :as str]
            [dorothy.core :as dot]
            [dorothy.jvm :as djvm]
            [hfdl.lang :refer [dataflow debug! heap-dump]]
            [missionary.core :as m])
  (:import (hfdl.impl.compiler Dataflow)))

(declare render-node)

(def ^:dynamic *visited?*) ;; Atom #{path …} track already explored paths

(def ^:dynamic *names*) ;; Atom {value -> name} to render human friendly names
                        ;; for opaque values

(def global-graph-props   {:compound :true, :splines :polyline})
(def global-edge-props    {:arrowhead :none, :concentrate :true})
(def global-cluster-props {:style :rounded})

(def default-link-props {:tailport  "s"
                         :arrowhead :odot
                         :dir       :forward})

(def invisible-node-props {:shape       :none
                           :label       ""
                           :peripheries 0
                           :height      0
                           :width       0
                           :style       :invis})

(def collection? `#{vector list hash-set hash-map})

(def delimiters `{vector   ["\\[" "\\]"]
                  list     ["' (" ")"]
                  hash-set ["#\\{" "\\}"]
                  hash-map ["{" "}"]
                  ::sexp   ["(" ")"]})

(def pretty-syms '[⓪ ① ② ③ ④ ⑤ ⑥ ⑦ ⑧ ⑨ ⑩ ⑪ ⑫ ⑬ ⑭ ⑮ ⑯ ⑰ ⑱ ⑲ ⑳ ㉑ ㉒ ㉓ ㉔ ㉕ ㉖ ㉗ ㉘ ㉙ ㉚ ㉛ ㉜ ㉝ ㉞ ㉟ ㊱ ㊲ ㊳ ㊴ ㊵ ㊶ ㊷ ㊸ ㊹ ㊺ ㊻ ㊼ ㊽ ㊾ ㊿])

(defn pretty
  "Return a UTF-8 Circled Number representation of `n`, or `n` itself if not
  found."
  [n]
  (str (if (<= 0 n (dec (count pretty-syms))) (get pretty-syms n) n)))

(defn str'
  "Like `str` but preserve nil."
  [& args]
  (apply str (replace {nil "nil"} args)))

(defn escape-str [s] (-> (str/replace s #"\{" "\\\\{")
                         (str/replace #"\}" "\\\\}")
                         (str/replace #"/>" "/\\\\>")))

(defn record "Render a collection to a record node"
  ([type coll] (record type nil coll))
  ([type label coll]
   (let [[open close] (delimiters type)]
     (cond-> ""
       (some? label) (str label "|")
       true          (str open (escape-str (apply str' coll)) close)))))

(defn gen-name! [prefix value]
  (let [names @*names*]
    (if-let [nom (get @*names* value)]
      nom
      (let [nom (str prefix " " (pretty (count names)))]
        (swap! *names* assoc value nom)
        nom))))

(defn render-value [value]
  (cond
    (instance? Dataflow value)                                                       (gen-name! "Dataflow" value)
    (str/starts-with? (str (class value)) "class missionary.core$watch$")            (gen-name! "Watch" value)
    (str/starts-with? (str (class value)) "class missionary.impl.Reactor$Publisher") (gen-name! "Constant" value)
    :else                                                                            (-> (str value)
                                                                                         (str/replace #"\"\\\"" "\"")
                                                                                         (str/replace #"\\\"\"" "\""))))

(defn label [program heap ip idx]
  (let [[type & args] (get program ip)
        value         (get-in heap [:heap [ip]])]
    (case type
      (:local :global)             (escape-str (str' (first args)))
      (:constant :variable :apply) (str (pretty idx) " = " (render-value value)))))

(defn intersperse
  "Like `interpose`, but also prepend and append `sep` in the result."
  [sep coll]
  (concat (interleave (repeat sep) coll) (list sep)))

(defn render-cell [program heap ip child-idx child-ip]
  (str "<" ip "_" child-idx "> " (label program heap child-ip child-ip)))

(defn render-hash-map-keys [program heap ip]
  (let [[_ _ args] (get program ip)]
    (map (fn [child-idx child-id] (render-cell program heap ip child-idx child-id))
         (range 0 (count args) 2)
         (take-nth 2 args))))

(defn render-hash-map-vals [program heap ip]
  (let [[_ _ args] (get program ip)]
    (map (fn [child-idx child-id] (render-cell program heap ip child-idx child-id))
         (range 1 (count args) 2)
         (take-nth 2 (next args)))) )

(defn value? [program ip]
  (#{:local :global} (first (get program ip))))

(defn render-hash-map [path program heap ip]
  (let [[_ _ args :as instr] (get program ip)]
    (concat
     ;; child nodes
     (mapcat (fn [child-ip]
               (when-not (value? program child-ip) (render-node path program heap child-ip))) args)
     ;; current node
     [[ip (cond-> {:label    (str (pretty ip)
                                  "|"
                                  (record `hash-map (interpose "|" (render-hash-map-keys program heap ip)))
                                  "|"
                                  (record `hash-map (interpose "|" (render-hash-map-vals program heap ip))))
                   :shape    :record
                   :penwidth 0.2}
            (:source (meta instr)) (assoc :cluster (:source (meta instr))))]]
     ;; links
     (map-indexed (fn [idx child-ip]
                    (when-not (value? program child-ip)
                      [child-ip ip (cond-> (assoc default-link-props :headport (str ip "_" idx (if (even? idx) ":w" ":e")))
                                     (contains? (:changes heap) [child-ip]) (assoc :color :red))]))
                  args))))

(defn render-form [path program heap ip]
  (let [[_ f-ip args-ip :as instr] (get program ip)
        [_ f-sym]                  (get program f-ip)
        f-sym                      (if (collection? f-sym) f-sym ::sexp)
        args-ip                    (if (collection? f-sym) args-ip (into [f-ip] args-ip))
        slot                       (find (:heap heap) [ip])]
    (concat
     ;; child nodes
     (mapcat (fn [arg-ip] (render-node path program heap arg-ip invisible-node-props)) args-ip)
     ;; current node
     [[ip (cond-> {:label    (cond->   (->> (map-indexed (fn [child-idx child-ip]
                                                       (render-cell program heap ip child-idx child-ip))
                                                     args-ip)
                                        (intersperse "|")
                                        (record f-sym (pretty ip)))
                               (some? slot) (str  " |= " (escape-str (render-value (val slot)))))
                   :shape    :record
                   :penwidth 0}
            (:source (meta instr)) (assoc :cluster (:source (meta instr))))]]
     ;; links
     (map-indexed (fn [child-idx child-ip]
                    (when-not (value? program child-ip)
                      [child-ip ip (cond-> (assoc default-link-props :headport (str ip "_" child-idx ":n"))
                                     (contains? (:changes heap) [child-ip]) (assoc :color :red))]))
                  args-ip))))

(defn render-apply-node [path program heap ip]
  (let [[_ f-ip _] (get program ip)
        [_ f-sym]  (get program f-ip)]
    (case f-sym
      clojure.core/hash-map (render-hash-map path program heap ip)
      (render-form path program heap ip))))

(defn subpath? [path-a path-b]
  (and (< (count path-a) (count path-b))
       (->> (map vector path-a path-b)
            (drop-while (fn [[a b]] (= a b)))
            (empty?))))

(defn focus-frame [m path]
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

(defn clusterized? [n]
  (some?
   (cond
     (node? n) (get-in n [1 :cluster])
     (link? n) (get-in n [2 :cluster]))))

(defn prefix-nodes [prefix nodes]
  (map (fn [n] (if (clusterized? n)
                n
                (cond
                  (node? n) (-> (assoc n 0 (render-prefix (conj prefix (first n))))
                                (update 1 assoc :cluster (render-prefix prefix)))
                  (link? n) (-> (assoc n 0 (render-prefix (conj prefix (first n))))
                                (assoc 1 (render-prefix (conj prefix (second n))))
                                (assoc-in [2 :cluster]  (render-prefix prefix)))
                  :else     n)))
        nodes))

(defn reactive-for? [program idx]
  (= `hfdl.lib/reactive-for (second (get program idx))))

(defn render-nested-frame [path program {:keys [heap changes]} ip]
  (let [[_ child-ip]  (get program ip)
        [type & args] (get program child-ip)]
    (mapcat identity
            (for [[fork frame] (focus-frame heap [ip])]
              (let [path'         (conj path ip fork)
                    program       (get frame [])
                    last-ip       (dec (count program))
                    nodes         (->> (binding [*visited?* (atom #{})]
                                         (render-node path'
                                                      program
                                                      {:heap frame :changes changes}
                                                      last-ip))
                                       (filter identity)
                                       (prefix-nodes path'))
                    cluster-id    (render-prefix path')
                    cluster-ident (str "cluster_" cluster-id)
                    link-props    (cond-> {:arrowhead :odot}
                                    (contains? changes [child-ip]) (assoc :color :red))]
                (conj [(dot/subgraph cluster-ident (cons {:label (str (case type
                                                                        (:local :global) (render-value (first args))
                                                                        (:apply)         (render-value (get heap [child-ip])))
                                                                      " fork " fork)
                                                          :style :rounded}
                                                         nodes))]
                      [(render-prefix (conj path' last-ip)) (render-prefix (conj path ip)) (assoc link-props :ltail cluster-ident, :cluster cluster-id)]
                      [(render-prefix (conj path child-ip))
                       (render-prefix (conj path' ip))
                       (assoc link-props :lhead cluster-ident, :cluster cluster-id)]))))))

(defn has-nested-frame? [heap ip]
  (->> (keys heap)
       (filter (fn [path] (subpath? [ip] path)))
       (not-empty)))

(defn render-node
  ([path program heap ip]
   (render-node path program heap ip {}))
  ([path program heap ip props]
   (when-not (@*visited?* ip)
     (swap! *visited?* conj ip)
     (let [[type & args :as instr] (get program ip)]
       (case type
         (:local :global)      [[ip (merge {:label (str (pretty ip) " " (label program heap ip nil)),
                                            :shape :none}
                                           props)]]
         (:variable :constant) (let [[child-ip] args]
                                 (concat [[ip (cond-> {:shape :none
                                                       :label (str (case type :variable "▽ ", :constant "△ ")
                                                                   (label program heap ip ip))}
                                                (:source (meta instr)) (assoc :cluster (:source (meta instr))))]]
                                         (render-node path program heap child-ip)
                                         (if (has-nested-frame? (:heap heap) ip)
                                           (render-nested-frame path program heap ip)
                                           [[child-ip ip (cond-> {:arrowhead :odot}
                                                           (contains? (:changes heap) [child-ip]) (assoc :color :red))]])))
         :apply                (render-apply-node path program heap ip))))))

(def cluster (comp :cluster props))

(defn clusters [nodes]
  (->> (filter node? nodes)
       (filter cluster)
       (group-by cluster)
       (map (fn [[cluster-id nodes]]
              (dot/subgraph (str "cluster_" cluster-id)
                            (cons (merge {:label cluster-id}
                                         global-cluster-props)
                                  (map (fn [[id _]] [id])
                                       nodes)))))))

(defn dot "Transform an HFIL `program` into dot representation"
  ([program] (dot program {} #{}))
  ([program process] (dot program process #{}))
  ([program process changes]
   (binding [*visited?* (atom #{})
             *names*    (atom {})]
     (let [instructions (:graph program)
           graph        (filter identity (render-node []
                                                      instructions
                                                      {:heap    (heap-dump process)
                                                       :changes changes}
                                                      (dec (count instructions))))]
       (concat (clusters graph) graph)))))

(defn heap-diff [ma mb]
  (reduce (fn [r [k v]]
            (if (or (not (contains? ma k))
                    (not= (get ma k) v))
              (conj r k)
              r))
          #{}
          mb))

(defn differential-dot [program & [process]]
  (let [prev (atom (heap-dump process))]
    (fn [process]
      (let [prev-heap @prev
            next-heap (heap-dump process)]
        (reset! prev next-heap)
        (dot program process (heap-diff prev-heap next-heap))))))

(defn compile [dot]
  (->> (cons [:edge global-edge-props] dot)
       (cons global-graph-props)
       (dot/digraph)
       (dot/dot)))

(defn show! [dot] (djvm/show! (compile dot)))

(defn save! [file dot] (djvm/save! (compile dot) file {:format :png}))

;;;;;;;;;;;;;;;;;;;;;;;
;; VISUAL TESTS ZONE ;;
;;;;;;;;;;;;;;;;;;;;;;;

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
    (show! (render @process))
    (swap! !a inc)
    (show! (render @process))
    )
  (do ; if
    (def !a (atom 1))
    (def a (m/watch !a))
    (def program (dataflow (if (odd? @a) :odd :even)))
    (def process (debug! program))
    (def render (differential-dot program))
    (show! (render @process))
    (swap! !a inc)
    (show! (render @process))
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
    (show! (render @process))
    (swap! !a inc)
    (show! (render @process))
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
    (save! "/home/geoffrey/Downloads/ds-pulse-1.png" (render @process))
    (swap! !a inc)
    (save! "/home/geoffrey/Downloads/ds-pulse-2.png" (render @process))
    (swap! !b inc)
    (show! (render @process))
    (save! "/home/geoffrey/Downloads/ds-pulse-3.png" (render @process))
    )

  (do ; fizzbuz
    (def !a (atom 0))
    (def a (m/watch !a))
    (def program (dataflow (let [n @a]
                             (cond
		                           (zero? (mod n 15)) "FizzBuzz"
		                           (zero? (mod n 3))  "Fizz"
		                           (zero? (mod n 5))  "Buzz"
		                           :else              n))))
    (def process (debug! program))
    (def render (differential-dot program))
    (show! (render @process))
    #_(dotimes [x 16]
      (reset! !a x)
      (save! (format "/home/geoffrey/Downloads/fizzbuz/%s.png" x) (render @process))))

  (do ; nested frames
    (def !input (atom "a"))
    (def >input (m/watch !input))
    (def grandchild (dataflow (str "Grandchild " @>input)))
    (def child (dataflow (str "Child " @grandchild)))
    (def parent (dataflow ["Parent" @child]))
    (def p (debug! parent))
    (def render (differential-dot parent))
    (show! (render @p))
    (reset! !input "b")
    (show! (render @p))

    ) ;; here
(do ; nested frame as a function
  (def !a (atom 1)) (def a (m/watch !a))
  (defn program1 [a] (dataflow (inc @a)))
  (def program2 (dataflow @(program1 a)))
  (def process (debug! program2))
  (show! (dot program2 @process)))

  ;; 1. Unhandled java.lang.IllegalArgumentException
  ;; No matching clause: :apply

  ;; viz.clj:  180  hfdl.viz/render-nested-frame


(comment
  (require '[hfdl.lib :refer [app]])
  (def p (debug! app))
  (show! (dot app @p) )
  (save! "/home/geoffrey/Downloads/reactive-for-2.png" (dot app @p) ))

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
