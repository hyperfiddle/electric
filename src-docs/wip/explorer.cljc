(ns wip.explorer
  (:require [clojure.set :refer [rename-keys]]
            [contrib.data :refer [auto-props unqualify]]
            [clojure.datafy :refer [datafy]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.photon-ui4 :as ui]
            [wip.gridsheet :as gridsheet :refer [GridSheet]]
            [hyperfiddle.rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros wip.explorer)))

(defn- -tree-list [depth xs children-fn keep? input]
  (eduction (mapcat (fn [x]
                      (let [x (datafy x)]
                        (if-let [children (children-fn x)]
                          (when-let [rows (seq (-tree-list (inc depth) children children-fn keep? input))]
                            (into [[depth x]] rows))
                          (cond-> [] (keep? x input) (conj [depth x]))))))
    (datafy xs)))

(defn tree-lister
  ([xs] (tree-lister xs (fn [_]) (fn [_ _] true)))
  ([xs children-fn keep?] (fn [input] (-tree-list 0 xs children-fn keep? input))))

(tests
  (vec ((tree-lister [1 2 [3 4] [5 [6 [7]]]] #(when (vector? %) %) (fn [v _] (odd? v))) nil))
  := [[0 1] [0 [3 4]] [1 3] [0 [5 [6 [7]]]] [1 5] [1 [6 [7]]] [2 [7]] [3 7]]

  ((tree-lister [{:dir "x" :children [{:file "a"} {:file "b"}]}] :children (fn [v needle] (-> v :file #{needle})) ) "a")
  (count (vec *1)) := 2

  "directory is omitted if there are no children matching keep?"
  ((tree-lister [{:dir "x" :children [{:file "a"} {:file "b"}]}] :children (fn [v needle] (-> v :file #{needle}))) "nope")
  (count (vec *1)) := 0)

(p/def cols nil)
(p/def Format (p/server (p/fn [row col] (pr-str (get row col))))) ; in div continuation

;; TODO move somewhere else or absorb
(p/defn BasicExplorer [props xs] ; o is an entity with recursive children
  (binding [gridsheet/Format Format]
    (GridSheet.
      xs
      (-> (auto-props (str *ns*) props {})
        (rename-keys {::row-height ::gridsheet/row-height
                      ::page-size ::gridsheet/page-size
                      ::columns ::gridsheet/columns})
        (->> (merge {::gridsheet/row-height 24
                     ::gridsheet/page-size 20
                     ::gridsheet/columns cols}))))))

(p/defn Explorer [title treelister props] ; o is an entity with recursive children
  (p/client
    (let [!search (atom "") search (p/watch !search)]
      #_(dom/dl
        (dom/dt (dom/text "scroll debug state"))
        (dom/dd (dom/pre (dom/text (pprint-str (update-keys (p/watch user.demo-scrollview/!scrollStateDebug) unqualify))))))
      (dom/div (dom/props {:class "hyperfiddle-explorer-title"}) title)
      (ui/input search (p/fn V! [v] (reset! !search v))
        (dom/props {:placeholder "Search files by name" :type "search"}))
      (dom/hr)
      (p/server
        (binding [gridsheet/Format Format]
          (GridSheet.
            #_RenderTableInfinite.
            #_TableSheet. ; deprecated, use page-size 100
            (treelister search)
            (-> (auto-props (str *ns*) props {})
                (rename-keys {::row-height ::gridsheet/row-height
                              ::page-size ::gridsheet/page-size
                              ::columns ::gridsheet/columns})
                (->> (merge {::gridsheet/row-height 24
                             ::gridsheet/page-size 20
                             ::gridsheet/columns cols})))))))))

; Sorting – we should not sort here, we must provide userland with sort directives
; to implement efficiently along with pagination directives
