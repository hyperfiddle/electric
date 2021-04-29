(ns hyperfiddle.client.edn-view.diff
  (:require ["@codemirror/state" :refer [EditorState ChangeSet]]
            ["@codemirror/view" :as view :refer [EditorView]]
            ["lezer-generator" :as lg]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [editscript.core :as editscript]
            [editscript.edit :as e]
            [nextjournal.clojure-mode :as cm-clj]
            [shadow.resource :as rc]))

(defn diff [a b]
  (e/get-edits (editscript/diff a b)))

(defn pprint-str [x] (str/trim (with-out-str (pprint x))))

(comment

  (def a {:a {:b 2
              :c 3}})

  (def b {:a {:b 3
              :c 4}})

  (def view (new EditorView #js{:state  (.create EditorState
                                                 #js{:doc        (pprint-str a)
                                                     :selection  js/undefined
                                                     :extensions #js[cm-clj/default-extensions]})
                                :parent (js/document.getElementById "hf-edn-test-view")}))

  (diff a b) ;; => [[[:a :b] :r 3] [[:a :c] :r 4]]

  (tree-seq coll? identity a)
  ;; => ({:a {:b 2, :c 3}} [:a {:b 2, :c 3}] :a {:b 2, :c 3} [:b 2] :b 2 [:c 3] :c 3)

  ;; Dev only!
  (require '[nextjournal.clojure-mode.node :as n])
  (def parser (lg/buildParser (rc/inline "./clojure.grammar")
                              #js{:externalProp n/node-prop}))

  (def tree (.parse parser (pprint-str a)))
  ;; => #object[Tree Program(Map("{",Keyword,Map("{",Keyword,Number,Keyword,Number,"}"),"}"))]
  )

(defn jump-nodes [^js cursor]
  (when (.next cursor)
    (loop [cursor cursor]
      (case (.. cursor -type -name)
        ("(" ")" "{" "}" "[" "]" "Operator" "\"")
        (when (.next cursor)
          (recur cursor))
        cursor))))

(defn find-node [^js ast, a, path]
  (let [^js cursor (.cursor ast)]
    (loop [[x & xs]          (tree-seq coll? identity a)
           [y & ys :as path] path]
      (if (nil? y)
        (jump-nodes cursor)
        (cond
          (= x y)
          (when (jump-nodes cursor)
            (recur xs ys))

          (and (sequential? x) (number? y))
          (do (dotimes [_ (inc y)]
                (jump-nodes cursor))
              (recur xs ys))

          (map-entry? x) (recur xs path) ;; Map Entries are not syntactic
          :else          (when (jump-nodes cursor)
                           (recur xs path)))))))

(defn- get-text [^js view, ^js node]
  (.. view -state -doc (sliceString (.-from node) (.-to node))))

(comment
  (find-node tree a (ffirst (diff a b)))
  (get-text view (find-node tree a (ffirst (diff a b))))
  (get-text view (find-node tree a (first (second (diff a b)))))
  )


(defn diff->change
  "Transform a clojure diff into a located text change using the ast."
  [^js node action value]
  (if node
    (let [from (.-from node)
          to   (.-to node)]
      (prn [from to action value])
      (case action
        :r #js{:from   from
               :to     to
               :insert (pprint-str value)}
        :- #js {:from from
                :to   to}
        :+ #js {:from   from
                :insert (pprint-str value)}))
    #js{:from   0
        :insert (pprint-str value)}))

(defn diffs->changes [^js ast, a, diffs]
  (map (fn [[path action value]]
         (diff->change (find-node ast a path) action value))
       diffs))

(comment
  (diffs->changes tree a (diff a b)))

(defn compose [^js doc, changes]
  (let [len (.-length doc)]
    (->> changes
         (map (fn [change] (.of ChangeSet change len)))
         (reduce (fn [^js a b] (.compose a b))
                 (.empty ChangeSet len)))))

(defn patch! [^js view, a, b]
  (let [changes       (diffs->changes (.. view -state -tree) a (diff a b))
        atomic-change (compose (.. view -state -doc) changes)]
    (.dispatch view #js{:changes atomic-change})))

(defn patcher []
  (let [next (volatile! nil)]
    (fn [^js view, new-val]
      (try
        (let [current (edn/read-string (.. view -state -doc (toString)))]
          (if (= current new-val)
            (when-let [next-val @next]
              (vreset! next nil)
              (patch! view current next-val))
            (patch! view current new-val)))
        (catch :default _
          (vreset! next new-val))))))

(comment
  (-> view .-state .-doc (.sliceString 0))
  (patch! view a b)
  )


