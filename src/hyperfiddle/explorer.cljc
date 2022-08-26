(ns hyperfiddle.explorer
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [user.util :refer [includes-str?]])
  #?(:cljs (:require-macros hyperfiddle.explorer)))

; all explorer bindings and p/fns must be called from server
(p/def children (constantly []))
(p/def cols nil)
(p/def search-attr) ; server
(p/def Format)
(p/def indent-level (p/client 0))

(p/def x)
(p/def m)
(p/def a)
(p/def v)

(p/defn Render-row [x m]
  (let [[a & as] cols]
    (p/client
      (dom/tr
        (dom/td {:style {:padding-left (-> indent-level (* 10) (str "px"))}}
          (p/server (binding [a a v (a m)] (Format. x m a v))))
        (p/server
          (p/for [a as]
            (-> (p/client (dom/td (p/server (binding [a a v (a m)] (Format. x m a v)))))
                (do nil)))))))) ; hack transfer issue

(p/def TreeTableBody')
(p/defn TreeTableBody [xs search]
  ; Returns a list of delayed render thunks that evaluate top-down recursively.
  (binding
    [#_#_cols (or cols (keys (first xs)))
     TreeTableBody' (p/fn [xs search] ; recur via binding until Photon gets proper recursion syntax
                      (->> (p/for [x (datafy xs)] ; weird java collections accepted, not just clojure colls
                             (let [m (datafy x)]
                               (if-let [xs (seq (children m))] ; if open
                                 (let [Xs (TreeTableBody'. xs search)] ; prewalk
                                   (when (seq Xs)
                                     ; don't render bottom up; thunk it for later top-down pass
                                     (p/fn []
                                       (binding [x x m m]
                                         (Render-row. x m)) ; omit folder row if no descendents matched filter
                                       (p/client
                                         (binding [indent-level (inc indent-level)]
                                           (p/server (p/for [X Xs] (X.))))))))
                                 (when (includes-str? (search-attr m) search) ; return nil to remove level
                                   (p/fn [] (binding [x x m m] (Render-row. x m)))))))
                           (remove nil?)))]
    (TreeTableBody'. xs search)))

(p/defn TreeTable [xs search]
  (p/client
    (dom/table
      ; need to datafy both xs and the first record to infer cols
      (dom/thead (p/for [k (p/server cols)] (dom/td (name k))))
      (dom/tbody
        (p/server
          (if-let [Xs (seq (TreeTableBody. xs search))]
            (p/for [X Xs] (X.))
            (p/client (dom/div {:class "no-results"} "no results matched"))))))))

(p/defn Explorer [xs title] ; o is an entity with recursive children
  (p/client
    (let [!search (atom "") search (p/watch !search)]
      (dom/div {:class "photon-demo-explorer"}
        (dom/div {:class "title"} (p/server title))
        (ui/input {::dom/placeholder "Search files by name" ::dom/type "search"
                   ::ui/input-event (p/fn [e] (reset! !search (.. e -target -value)))})
        (dom/hr)
        (p/server (TreeTable. xs search))))))
