(ns wip.hfql-links
  (:require #?(:clj dev)
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui :as ui]
            [hyperfiddle.photon-ui :as photon-ui]
            [wip.orders :refer [orders genders shirt-sizes one-order]]
            [hyperfiddle.hfql.router :as router])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.hfql-links)))


(defonce flag (atom false))

#?(:cljs (when-not @flag
           (reset! flag true)
           (reset! hf/route-state `((wip.orders/orders "alice"))))) ;; set initial (client-side) route state

(p/defn App []
  (dom/div {:id "main", :class "browser"}
    (let [[current-route prev] (p/watch hf/route-state)]
      (dom/div {:class "view"}
        (photon-ui/button {:dom/disabled           (not (some? prev))
                           :dom/style              {:grid-row 1, :justify-self :flex-start}
                           ::photon-ui/click-event (p/fn [e] () (when e (hf/navigate-back!)))}
             (dom/text "< " (some-> (first prev) name)))
        (p/server
          (router/router current-route "./hfql_links.edn"))))))

(def main
  #?(:cljs (p/boot
             (try (binding [dom/node (dom/by-id "root")]
                    (p/server
                      (binding [hf/db     hf/*db*
                                hf/Render ui/Render]
                        (ui/with-spec-render
                          (p/client (App.))))))
                  (catch Pending _)))))

(comment
  #?(:clj (user/browser-main! `main))
  )
