(ns user.demo-7-explorer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [user.util :refer [includes-str?]])
  #?(:cljs (:require-macros user.demo-7-explorer)))

; wrap java interop (hack)
(defn file-is-dir [file-handle] #?(:clj (.isDirectory file-handle)))
(defn file-is-file [file-handle] #?(:clj (.isFile file-handle)))
(defn file-list-files [file-handle] #?(:clj (.listFiles file-handle)))
(defn file-get-name [file-handle] #?(:clj (.getName file-handle)))

; hack, Photon compiler doesn't implement recursion yet
(p/def Recur)

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Explorer"))
    (let [s (ui/input {:type :search, :placeholder "Filter…", :style {:width "20rem"}})]
      (p/server
        (binding [Recur (p/fn [handle]
                          (cond
                            (file-is-dir handle)
                            (p/client (dom/li (dom/text (p/server (file-get-name handle)))
                                              (dom/ul (p/server (p/for [x (file-list-files handle)]
                                                                  (Recur. x))))))
                            (file-is-file handle)
                            (when (p/deduping (includes-str? (file-get-name handle) s))
                              (p/client (dom/li (dom/text (p/server (file-get-name handle))))))))]
          (p/client (dom/ul (p/server (Recur. (clojure.java.io/file "src"))))))))))
