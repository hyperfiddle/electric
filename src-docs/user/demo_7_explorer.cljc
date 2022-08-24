(ns user.demo-7-explorer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            clojure.datafy
            clojure.core.protocols
            [user.util :refer [includes-str? pprint-str]])
  #?(:clj (:import java.nio.file.Paths
                   java.nio.file.Files
                   java.io.File
                   java.nio.file.LinkOption
                   java.nio.file.attribute.BasicFileAttributes))
  #?(:cljs (:require-macros user.demo-7-explorer)))

(comment
  (def h (clojure.java.io/file "node_modules/"))
  (.getName h)
  (.isDirectory h)
  (.isFile h)
  (.listFiles h)
  (for [x (take 5 (.listFiles h))]
    (.getName x))
  (.getPath h))

#?(:clj
   (extend-protocol clojure.core.protocols/Datafiable
     java.io.File
     (datafy [o]
       (let [m (-> o .getAbsolutePath (java.nio.file.Paths/get (make-array String 0))
                   (java.nio.file.Files/readAttributes java.nio.file.attribute.BasicFileAttributes (make-array java.nio.file.LinkOption 0)))]
         {#_#_::creationTime (-> m .creationTime .toInstant java.util.Date/from)
          #_#_::lastAccessTime (-> m .lastAccessTime .toInstant java.util.Date/from)
          ::lastModifiedTime (-> m .lastModifiedTime .toInstant java.util.Date/from)
          ::isDirectory (if (.isDirectory m) :folder :file)
          ;::isRegularFile (if-not (.isRegularFile m) :not-a-regular-file)
          ;::isSymbolicLink (if (.isSymbolicLink m) :link)
          ;::isOther (if (.isOther m) :other nil)
          ::size (.size m)}))))

(comment
  (clojure.datafy/datafy h)
  (meta (clojure.datafy/datafy h)))

; all bindings and p/fns are are server colored/biased
(p/def indent-level (p/client 0))
(p/defn Indent [Xs]
  (p/client
    (binding [indent-level (inc indent-level)]
      ; top-down render pass
      (p/server (p/for [X Xs]
                  (X.))))))

#?(:cljs (defn format-value [k v]
           (case k
             (::creationTime ::lastAccessTime ::lastModifiedTime)
             (.toLocaleDateString v)
             (str v))))

(p/defn Render-row [h]
  (p/client
    (dom/tr
      (dom/td {:style {:padding-left (-> indent-level (* 10) #_(+ 10) (str "px"))}}
        (p/server (.getName h)))
      (p/server
        (do (p/for [[k v] (clojure.datafy/datafy h)]
              (p/client (dom/td (format-value k v))))
            nil)))))

; Improvements
; Native search
; nav protocol
; reduce amount of dom nodes (avoid p/hook)
;   grid instead of table
;   lazy folding/unfolding directories (no need for pagination)

(p/def Tree)
(p/defn ExplorerTable [h s] ; server biased to prevent accidental handle transfer (must fix transfer)
  (assert (.isDirectory h) (str "file handle: " (.getName h) " is not a directory"))
  (binding [Tree (p/fn [h]
                   (->> (p/for [h (.listFiles h)]
                          (cond (.isDirectory h) (let [Xs (Tree. h)] ; prewalk
                                                   (when (seq Xs)
                                                     ; don't render bottom up; thunk it for later top-down pass
                                                     (p/fn []
                                                       (Render-row. h) ; omit folder row if no descendents matched filter
                                                       (Indent. Xs))))
                                (.isFile h) (let [fname (.getName h)]
                                              (when (includes-str? fname s) ; return nil to remove level
                                                (p/fn [] (Render-row. h))))))
                        (remove nil?)))]

    ;(Render-row. h) -- never render target folder

    (if-let [xs (seq (Tree. h))] ; no when check is what makes this different
      (Indent. xs)
      (p/client (dom/div "no results matched")))))

(p/defn Explorer [resource-folder] ; server bias
  (let [h (clojure.java.io/file resource-folder)]
    (p/client
      (let [!search (atom "") search (p/watch !search)]
        (dom/div {:class "photon-demo-explorer"}
          (dom/div (p/server (.getAbsolutePath h)))
          (ui/input {::dom/placeholder "Search files by name" ::dom/type "search" :style {:width "40rem"}
                     ::ui/input-event (p/fn [e] (reset! !search (.. e -target -value)))})
          (dom/hr)
          (dom/table
            (p/server (ExplorerTable. h search))))))))

(def !target #?(:cljs (atom "src") :clj nil)) (p/def target (p/client (p/watch !target)))

(p/defn App []
  (dom/div
    (dom/h1 "Folder Explorer")
    (dom/element dom/style ".photon-demo-explorer tr:nth-child(even) { background-color: #f2f2f2; }")
    ;(dom/element dom/style ".photon-demo-explorer td:not(:first-child) { width: 10rem; }")
    (dom/element dom/style ".photon-demo-explorer td { padding-left: 3px; padding-right: 3px; }")
    #_
    (dom/div "Folder: "
      (ui/button {::ui/click-event (p/fn [e] (reset! !target "src"))} "src")
      (ui/button {::ui/click-event (p/fn [e] (reset! !target "node_modules"))} "node_modules (todo)"))
    (p/server (Explorer. (p/client target)))))
