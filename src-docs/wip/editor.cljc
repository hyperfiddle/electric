(ns wip.editor
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            #?(:clj [clojure.java.io :as io]))
  (:import (hyperfiddle.photon Pending)
           (missionary Cancelled)
           #?(:clj (java.nio.file WatchService Paths FileSystems))))

#?(:clj (def ENTRY_MODIFY java.nio.file.StandardWatchEventKinds/ENTRY_MODIFY))

#?(:clj (defn register! [dir watcher]
          (let [;; File change notification might take several seconds on macos.
                ;; Set watch event priority to high to be notfied ASAP.
                modifiers (when-let [modifier (try
                                                (let [c (Class/forName "com.sun.nio.file.SensitivityWatchEventModifier")
                                                      f (.getField c "HIGH")]
                                                  (.get f c))
                                                (catch Exception _ nil))]
                            (doto (make-array java.nio.file.WatchEvent$Modifier 1)
                              (aset 0 modifier)))
                types     (into-array [ENTRY_MODIFY])]
            (if modifiers
              (.register dir watcher (into-array types) modifiers)
              (.register dir watcher (into-array types))))))

#?(:clj (defn watch-dir! [dir-path callback]
          (let [dir     (Paths/get dir-path (make-array String 0))
                watcher (.. FileSystems getDefault newWatchService)]
            (register! dir watcher)
            (letfn [(watch [watcher keys]
                      (let [key (.take watcher)]
                        (doseq [event (.pollEvents key)]
                          (let [name (->> event .context (.resolve dir) str)]
                            (callback name)
                            (.reset key)))
                        (recur watcher keys)))]
              (future (watch watcher keys))
              #(.close watcher)))))

(defn file-watcher [file-path]
  #?(:clj
     (let [dir-path (.getParent (io/file file-path))]
       (->> (m/observe (fn [!]
                         (let [stop (watch-dir! dir-path
                                                (fn [filename]
                                                  (prn dir-path filename)
                                                  (when (= filename file-path)
                                                    (! (slurp filename)))))]
                           #(stop))))
            (m/eduction (dedupe))
            (m/reductions {} (slurp file-path))
            (m/relieve {})))))

(defn write! [dir-path text]
  #?(:clj (spit dir-path text)))

(defn debounce [delay flow]
  (m/relieve {} (m/reductions {} nil (m/ap (let [x (m/?< flow)]
                                             (try (m/? (m/sleep delay x))
                                                  (catch Cancelled _ (m/amb))))))))

(def main #?(:cljs (p/client
                    (p/main
                     (try (binding [dom/parent (dom/by-id "root")]
                            ~@(let [dir-path "src-docs/user/demo_healthcheck.cljc"
                                    content  (new (file-watcher dir-path))
                                    edited   ~@(dom/div {:style {:width "100vw"}}
                                                        (dom/textarea {:rows  25
                                                                       :style {:width "100%"}}
                                                                    (dom/text content)
                                                                    (new (->> (dom/events "input" (map dom/target-value))
                                                                              (debounce 2000)))))]
                                (when (and edited (not= edited content))
                                  (write! dir-path edited))))
                          (catch Pending _))))))

(comment
  (user/browser-main! `main)
  )
