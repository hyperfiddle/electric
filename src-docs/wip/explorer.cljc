(ns wip.explorer
  (:require [clojure.set :refer [rename-keys]]
            [contrib.data :refer [auto-props unqualify]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.router :as router]
            [wip.gridsheet :as gridsheet :refer [GridSheet]]))

(e/def cols nil)
(e/def Format (e/server (e/fn [row col] (pr-str (get row col))))) ; in div continuation

(e/defn Explorer [query-fn props] ; o is an entity with recursive children
  (e/client
    (let [{:keys [::search] :as s} router/route]
      #_(dom/dl
        (dom/dt (dom/text "scroll debug state"))
        (dom/dd (dom/pre (dom/text (pprint-str (update-keys (e/watch user.demo-scrollview/!scrollStateDebug) unqualify))))))
      (ui/input search (e/fn V! [v] (router/swap-route! assoc ::search v)) ; todo (swap! router/!route assoc ::search v)
        (dom/props {:placeholder "Search" :type "search"}))
      (dom/hr)
      (e/server
        (binding [gridsheet/Format Format]
          (GridSheet.
            (query-fn search)
            (-> (auto-props (str *ns*) props {})
                (rename-keys {::row-height ::gridsheet/row-height
                              ::page-size ::gridsheet/page-size
                              ::columns ::gridsheet/columns})
                (->> (merge {::gridsheet/row-height 24
                             ::gridsheet/page-size 20
                             ::gridsheet/columns cols})))))))))

; Sorting – we should not sort here, we must provide userland with sort directives
; to implement efficiently along with pagination directives
