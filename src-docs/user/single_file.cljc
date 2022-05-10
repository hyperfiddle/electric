(ns user.single-file
  (:require [hyperfiddle.photon :as p]
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
  #?(:cljs (p/client
             (p/main
               (binding [dom/parent (dom/by-id "root")]
                 (dom/div
                   (dom/attribute "id" "main")
                   (dom/class "browser")
                   (dom/div
                     (dom/class "view")
                     (App.))))))))

(def reactor)

(defn ^:dev/before-load stop! []
  #?(:cljs (do (when reactor (reactor)) ; teardown
             (set! reactor nil))))

(defn ^:dev/after-load ^:export start! []
  #?(:cljs (set! reactor (main js/console.log js/console.error))))

(def build-config {:build-id      :app
                   :target        :browser
                   :devtools      {:watch-dir "resources/public"} ;; live reload CSS
                   :build-options {:cache-level :jars} ;; Recompile everything but jars.
                   :output-dir    "resources/public/js"
                   :asset-path    "/js"
                   :modules       {:main {:entries   ['user.single-file]
                                          :append-js (str (munge 'user.single-file) ".start_BANG_();")}}})

#?(:clj (require 'shadow.cljs.devtools.server 'shadow.cljs.devtools.api))
#?(:clj (shadow.cljs.devtools.server/start!))
#?(:clj (shadow.cljs.devtools.api/watch build-config)) ;; Assets are served by shadow
#?(:clj (p/start-server!)) ;; Websocket only