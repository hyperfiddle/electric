(ns wip.editor
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui.codemirror :as codemirror]
            [missionary.core :as m]
            [hyperfiddle.zero :as z])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.editor)))

(defn write! [dir-path text] #?(:clj (spit dir-path text)))
(defn read! [file-path] #?(:clj (slurp file-path)))

(def file "src-docs/wip/hfql_links.edn")

(def !ack (atom 0))
(p/def ack (p/watch !ack))


(def main
  #?(:cljs
     (p/client
      (p/main
       (try (binding [dom/node (dom/by-id "root")]
              ~@(let [content (read! file)]
                  ~@(let [text (dom/div {:style {:width "100vw"}}
                                        (let [text (new codemirror/string content)]
                                          text
                                          (z/impulse ~@ack (dom/>keychord-events #{"meta+s" "ctrl+s"}
                                                                                 (map (constantly text))))))]
                      (prn "Text" text)
                      (when text
                        ~@(do (swap! !ack inc)
                              (write! file text))))))
            (catch Pending _))))))

(comment
  (user/browser-main! `main)
  )
