(ns user.demo-7-explorer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [user.util :refer [includes-str? pprint-str]])
  #?(:cljs (:require-macros user.demo-7-explorer)))

(comment
  (def h (clojure.java.io/file "node_modules/"))
  (.getName h)
  (.isDirectory h)
  (.isFile h)
  (.listFiles h)
  (for [x (take 5 (.listFiles h))]
    (.getName x))
  )

; all bindings and p/fns are are server color
(p/defn Indent [Xs]
  (p/client
    (dom/ul (p/server (p/for [X Xs] ; files and folders
                        (p/client (dom/li (p/server (X.)))))))))

(p/defn File [h] (p/client (dom/text (p/server (.getName h)))))
(p/defn Folder [h] (p/client (dom/text (p/server (.getName h)))))

(p/def Tree)
(p/defn Explorer [h s] ; server biased to prevent accidental handle transfer (must fix transfer)
  (assert (.isDirectory h) (str "file handle: " (.getName h) " is not a directory"))
  (binding [Tree (p/fn [h]
                   (->> (p/for [h (.listFiles h)]
                          (cond (.isFile h) (let [fname (.getName h)]
                                              (when (includes-str? fname s) ; return nil to remove level
                                                (p/fn [] (File. h)))) ; file render continuation
                                (.isDirectory h) (let [xs (Tree. h)]
                                                   (when (seq xs)
                                                     (p/fn [] ; folder render continuation
                                                       (Folder. h) ; skip empty folders
                                                       (Indent. xs))))))
                        (remove nil?)))]
    (Folder. h) ; never skip even if empty
    (Indent. (Tree. h)))) ; no when check is what makes this different

(def !target #?(:cljs (atom "src") :clj nil)) (p/def target (p/client (p/watch !target)))
(def !search #?(:cljs (atom "") :clj nil)) (p/def search (p/client (p/watch !search)))

(p/defn App []
  (dom/div
    (dom/h1 "Folder Explorer")
    (dom/div "Folder: "
      (ui/button {::ui/click-event (p/fn [e] (reset! !target "src"))} "src")
      (ui/button {::ui/click-event (p/fn [e] (reset! !target "node_modules"))} "node_modules (todo)"))
    (dom/p "Try typing \"compiler\"")
    (ui/input {::dom/placeholder "Search files by name" ::dom/type "search" :style {:width "40rem"}
               ::ui/input-event  (p/fn [e] (reset! !search (.. e -target -value)))})
    (p/server (Explorer. (clojure.java.io/file (p/client target)) (p/client search)))))

(comment
  !search
  (reset! !search "LICENSE")
  (reset! !search "")
  )
