(ns wip.explorer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [user.util :refer [includes-str?]])
  (:import (hyperfiddle.photon Pending)
           (missionary Cancelled))
  #?(:cljs (:require-macros wip.explorer)))

(p/defn Input []
  (dom/input {:type :search, :placeholder "Filter…"}
             (dom/events "input" (map (dom/oget :target :value)) "")))

; wrap java interop (hack)
(defn file-is-dir [file-handle] #?(:clj (.isDirectory file-handle)))
(defn file-is-file [file-handle] #?(:clj (.isFile file-handle)))
(defn file-list-files [file-handle] #?(:clj (.listFiles file-handle)))
(defn file-get-name [file-handle] #?(:clj (.getName file-handle)))
(defn file-get-absolute-path [file-handle] #?(:clj (.getAbsolutePath file-handle)))

(p/def Foo)

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Explorer"))
    (let [s (Input.)]
      ~@(binding [Foo (p/fn [f]
                        (cond
                          (file-is-dir f)
                          ~@(dom/li (dom/text ~@(file-get-name f))
                                    (dom/ul
                                      ~@(p/for [x (file-list-files f)]
                                          (Foo. x))))
                          (file-is-file f)
                          (when (includes-str? (file-get-name f) s)
                            ~@(dom/li (dom/text ~@(file-get-name f))))))]
          ~@(dom/ul ~@(Foo. (clojure.java.io/file "src")))))))

(def main #?(:cljs (p/client (p/main
                               (try
                                 (binding [dom/node (dom/by-id "root")]
                                   (App.))
                                 (catch Pending _)
                                 (catch Cancelled _))))))

(comment
  #?(:clj (user/browser-main! `main))
  )
