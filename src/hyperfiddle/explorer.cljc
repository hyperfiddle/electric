(ns hyperfiddle.explorer
  (:require [clojure.set :refer [rename-keys]]
            [contrib.data :refer [auto-props unqualify]]
            [clojure.datafy :refer [datafy]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.gridsheet :as gridsheet :refer [GridSheet]]
            [hyperfiddle.rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros hyperfiddle.explorer)))

(defn includes-str? [m s]
  ; org.apache.commons.lang3.StringUtils.containsIgnoreCase()
  ; common utility for explorer search,
  ; note this should be done by the database though
  (clojure.string/includes? (clojure.string/lower-case (str m))
                            (clojure.string/lower-case (str s))))

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
  (count (vec *1)) := 0
  )

(p/def cols nil)
(p/def Format (p/server (p/fn [row col] (pr-str (get row col)))))

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
        (dom/dt "scroll debug state")
        (dom/dd (dom/pre (pprint-str (update-keys (p/watch hyperfiddle.scrollview/!scrollStateDebug) unqualify)))))
      (dom/div {:class "hyperfiddle-explorer-title"} title)
      (ui/input {::dom/placeholder "Search files by name" ::dom/type "search"
                 ::ui/input-event (p/fn [e] (reset! !search (.. e -target -value)))})
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
