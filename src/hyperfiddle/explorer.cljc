(ns hyperfiddle.explorer
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [user.util :refer [includes-str?]])
  #?(:cljs (:require-macros hyperfiddle.explorer)))

; all explorer bindings and p/fns must be called from server
(p/def children)
(p/def cols nil)
(p/def search-attr) ; server
(p/def Format)
(p/defn Title [m] (pr-str m))
(p/def indent-level (p/client 0))

(p/defn Render-row [m]
  (let [[namek & ks] cols]
    (p/client
      (dom/tr
        (dom/td {:style {:padding-left (-> indent-level (* 10) (str "px"))}}
          (p/server (namek m)))
        (p/server
          (p/for [k ks]
            (-> (p/client (dom/td (p/server (Format. k (k m)))))
                (do nil)))))))) ; hack transfer issue

(p/defn Indent [Xs]
  (p/client
    (binding [indent-level (inc indent-level)]
      ; top-down render pass
      (p/server (p/for [X Xs] ; rows at this indent level
                  (X.))))))

(p/def Tree)
(p/defn Explorer [h]
  (let [m (datafy h)]
    (assert (children m))
    (p/client
      (let [!search (atom "") search (p/watch !search)]
        (dom/div {:class "photon-demo-explorer"}
          (dom/div {:class "title"} (p/server (Title. m)))
          (ui/input {::dom/placeholder "Search files by name" ::dom/type "search"
                     ::ui/input-event (p/fn [e] (reset! !search (.. e -target -value)))})
          (dom/hr)
          (dom/table
            (p/server
              (binding [cols (or cols (keys m))
                        Tree (p/fn [m] ; recur via binding until Photon gets proper recursion syntax
                               (->> (p/for [x (datafy (nav m children (children m)))]
                                      (let [m (datafy x)]
                                        (if (children m)
                                          (let [Xs (Tree. m)] ; prewalk
                                            (when (seq Xs)
                                              ; don't render bottom up; thunk it for later top-down pass
                                              (p/fn []
                                                (Render-row. m) ; omit folder row if no descendents matched filter
                                                (Indent. Xs))))
                                          (when (includes-str? (search-attr m) search) ; return nil to remove level
                                            (p/fn [] (Render-row. m))))))
                                    (remove nil?)))]

                (p/client
                  (dom/thead (p/for [k (p/server cols)] (dom/td (name k))))
                  (dom/tbody
                    ;(Render-row. h) -- never render target folder
                    (p/server
                      (if-let [Xs (seq (Tree. m))]
                        (Indent. Xs)
                        (p/client (dom/div {:class "no-results"} "no results matched"))))))))))))))
