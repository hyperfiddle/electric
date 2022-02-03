(ns leo.file-watcher
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [missionary.core :as m])
  (:import
    (java.nio.file Path FileSystems Paths WatchEvent$Modifier StandardWatchEventKinds
                   StandardWatchEventKinds$StdWatchEventKind WatchEvent)
    (com.sun.nio.file SensitivityWatchEventModifier)
    (java.io File PushbackReader)))

(defn path [s & ss]
  (Paths/get ^String s (into-array String ss)))

(def events [StandardWatchEventKinds/ENTRY_MODIFY])
(def modifiers [SensitivityWatchEventModifier/HIGH])

(defn watch-dir "
Returns a discrete flow watching given directory (as a Path). Whenever a file is modified in this directory, the Path
of this file is emitted."
  [^Path dir]
  (m/ap
    (let [ws (.newWatchService (FileSystems/getDefault))
          key (.register dir ws
                         (into-array StandardWatchEventKinds$StdWatchEventKind events)
                         (into-array WatchEvent$Modifier modifiers))]
      (try
        (loop []
          (m/? (m/via m/blk (.take ws)))
          (m/amb> (.context ^WatchEvent (m/?> (m/seed (.pollEvents key))))
                  (do (.reset key) (recur))))
        (catch Throwable e
          (.cancel key)
          (throw e))))))

(comment
  (def it ((watch-dir (path "./")) #(prn :ready) #(prn :done)))
  @it
  (it))

(defn read-edn-forms [^File file]
  (m/via m/blk
         (try
           (with-open [r (PushbackReader. (io/reader file))]
             {:status :success
              :forms  (into [] (take-while (complement #{r}))
                            (repeatedly #(edn/read {:eof r} r)))})
           (catch RuntimeException e
             {:status  :failure
              :message (ex-message e)})
           (catch InterruptedException _
             {:status :pending}))))

(defn watch-file
  "Returns a continuous flow watching given file and reading edn forms."
  [^File file]
  (let [path (.toPath file)]
    (->> (m/ap
           (m/?< (->> (watch-dir (.getParent (Paths/get (.toURI file))))
                      (m/eduction (filter #{path}))
                      (m/reductions {} path)))
           (m/? (read-edn-forms file)))
         (m/reductions {} {:status :pending})
         (m/relieve {}))))

(comment
  (def it ((watch-file (io/file "hyperfiddle.edn")) #(prn :ready) #(prn :done)))
  @it
  (it))