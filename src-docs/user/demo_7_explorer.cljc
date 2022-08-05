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

(p/def FooRecur)
(p/defn Foo [handle s]
  ; hacked recursion workaround (Photon compiler hasn't implemented recursion yet)
  (binding [FooRecur (p/fn [handle s]
                       (cond
                         (file-is-dir handle)
                         (p/client (dom/li (dom/text (p/server (file-get-name handle)))
                                           (dom/ul (p/server (p/for [x (file-list-files handle)]
                                                               (FooRecur. x s))))))
                         (file-is-file handle)
                         (when (p/deduping (includes-str? (file-get-name handle) s))
                           (p/client (dom/li (dom/text (p/server (file-get-name handle))))))))]
    (FooRecur. handle s)))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Explorer"))
    (let [!s (atom "") s (p/watch !s)]
      (ui/input {::dom/placeholder "Filter" ::dom/type "search" :style {:width "20rem"}
                 ::ui/input-event (p/fn [e] (reset! !s (:value dom/node)))})
      (dom/ul (p/server (Foo. (clojure.java.io/file "src") s))))))
