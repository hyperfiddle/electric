;; Run this file with `clj -X:devkit :ns user.single-file`

(ns user.single-file
  (:require [hyperfiddle.client :refer [client]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [user.orders :refer [orders]]
            [hyperfiddle.api :as hf]
            [hyperfiddle.ui :as ui]))

(p/defn App []
  (dom/div (dom/text ~@(ui/with-spec-render
                         (binding [hf/db hf/*db*
                                   hf/render ui/render]
                           (hf/hfql {(orders .) [:order/email]}))))))

(def main
  (client
   (p/main
    (binding [dom/parent (dom/by-id "root")]
      (dom/div
       (dom/attribute "id" "main")
       (dom/class "browser")
       (dom/div
        (dom/class "view")
        (App.)))))))

(def ^:export reactor)

(defn ^:dev/before-load stop! []
  (when reactor (reactor)) ; teardown
  (set! reactor nil))

(defn ^:dev/after-load ^:export start! []
  (set! reactor (main js/console.log js/console.error)))