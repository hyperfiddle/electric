(ns wip.editor
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom3 :as dom]
            [hyperfiddle.ui.codemirror :as codemirror])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.editor)))

(defn write! [dir-path text] #?(:clj (spit dir-path text)))
(defn read! [file-path] #?(:clj (slurp file-path)))

(def file "src-docs/wip/hfql_links.cljc")

(def main
  #?(:cljs
     (p/client
      (p/main
       (try (binding [dom/parent (dom/by-id "root")]
              ~@(let [content (read! file)
                      edited ~@(dom/div {:style {:width "100vw"}}
                                        (p/debounce 2000 (new codemirror/string content)))]
                  (when edited
                    (write! file edited))))
            (catch Pending _))))))

(comment
  (user/browser-main! `main)
  )
