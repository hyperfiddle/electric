(ns wip.hfql-links
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui :as ui]
            [wip.orders :refer [orders genders shirt-sizes one-order]]
            [hyperfiddle.hfql.router :as router]
            [shadow.resource :as res])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.hfql-links)))


(defonce flag (atom false))

#?(:cljs (when-not @flag
           (reset! flag true)
           (reset! hf/route-state `((wip.orders/orders "alice"))))) ;; set initial (client-side) route state

(p/defn App []
  (dom/div {:id "main", :class "browser"}
   (let [[current prev] (p/watch hf/route-state)]
     (dom/div {:class "view"}
      (dom/button {:disabled (not (some? prev))
                   :style    {:grid-row 1, :justify-self :flex-start}}
             (dom/text (str "< " (some-> (first prev) name)))
             (dom/events "click" (map hf/navigate-back!)))
      (let [route current]
        ~@(router/router route "./hfql_links.edn"))
      ))))

(def main
  #?(:cljs (p/boot
             (try (binding [dom/node (dom/by-id "root")]
                    ~@(binding [hf/db     hf/*db*
                                hf/Render ui/Render]
                        (ui/with-spec-render
                          ~@(App.))))
                  (catch Pending _)))))

(comment
  #?(:clj (user/browser-main! `main))
  )
