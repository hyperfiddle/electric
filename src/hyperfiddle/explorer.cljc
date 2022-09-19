(ns hyperfiddle.explorer
  (:require [clojure.set :refer [rename-keys]]
            [contrib.data :refer [unqualify auto-props]]
            [clojure.datafy :refer [datafy]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.gridsheet :as gridsheet :refer [GridSheet RenderTableInfinite]]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [user.util :refer [includes-str? pprint-str]])
  #?(:cljs (:require-macros hyperfiddle.explorer)))

; all explorer bindings and p/fns must be called from server
(p/def Children (p/fn [m] nil))
(p/def Search? (p/fn [m s] (includes-str? m s))) ; could be clojure fn

(p/def TreeList')
(p/defn TreeList [xs needle]
  ; Pre-walk the tree and filter bottom-up, to omit layers with no descendents that match.
  ; Returns a flattened list of [depth x] for linear pagination and rendering.
  (binding [TreeList' (p/fn [depth xs needle] ; recur via binding until Photon gets proper recursion
                        (->> (p/for [x (datafy xs)] ; xs can be an intrusive collection
                               (let [m (datafy x)]
                                 ; prewalk, omit parent if no descendents matched
                                 (if-let [xs (seq (Children. m))] ; if open
                                   ; omit level if no descendents matched filter
                                   (when-let [rows (seq (TreeList'. (inc depth) xs needle))]
                                     (cons [depth m] rows)) ; expand children inline
                                   (when (Search?. m needle) [[depth m]])))) ; filter bottom up
                             (mapcat identity)))]
    (TreeList'. 0 xs needle)))

#?(:clj
   (tests
     (with (p/run (tap (binding [Children (p/fn [x] (if (vector? x) x))
                               Search? (p/fn [x needle] (odd? x))]
                       (TreeList. [1 2 [3 4] [5 [6 [7]]]] ""))))
       % := [[0 1] [0 [3 4]] [1 3] [0 [5 [6 [7]]]] [1 5] [1 [6 [7]]] [2 [7]] [3 7]])))

(p/def cols nil)
(p/def Format (p/server (p/fn [m a v] (pr-str v))))

(p/defn Explorer [title xs props] ; o is an entity with recursive children
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
            (TreeList. xs search)
            (-> (auto-props (str *ns*) props {})
                (rename-keys {::row-height ::gridsheet/row-height
                              ::page-size ::gridsheet/page-size
                              ::columns ::gridsheet/columns})
                (->> (merge {::gridsheet/row-height 24
                             ::gridsheet/page-size 20
                             ::gridsheet/columns cols})))))))))
