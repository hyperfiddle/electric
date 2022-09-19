(ns dustin.y2022.file-watcher
  (:require [clojure.java.io :as io]
            [clojure.java.data :as j]
            [clojure.datafy :refer [datafy]]
            [leo.file-watcher :refer [watch-dir
                                      read-edn-forms
                                      watch-file
                                      path]]
            [hyperfiddle.rcf :as rcf :refer [tests % tap]]
            [missionary.core :as m]))

(def !x (atom nil))

(comment
  (slurp (io/file "deps.edn"))
  (slurp (io/file "scratch/dustin/y2022/file_watcher.clj"))

  ; Path
  ; getParent, getNameCount, getFileName, getName, subpath,

  (def x @!x)
  (bean x)
  (datafy x)
  (j/from-java x)
  (str x)

  (-> (path "./")
      (.toAbsolutePath)
      (str))

  (def cancel
    ((m/reactor
       (m/stream!
         (m/ap
           (let [x (m/?< (watch-dir (path "./")))]
             #_(reset! !x x)
             (println (str x)))))
       (m/stream! (m/ap (println (m/?< (watch-file (io/file "bro.edn")))))))
     prn prn))
  (cancel)

  (def it ((watch-file (io/file "scratch/dustin/y2022/file_watcher.clj"))
           #(prn :ready) #(prn :done)))
  @it
  (it)

  (def it (#(prn :ready) #(prn :done)))
  @it
  (it)

  )

