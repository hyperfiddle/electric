(ns user.demo-7-explorer
  (:require [clojure.datafy :refer [datafy]]
            [clojure.core.protocols :refer [nav]]
            #?(:clj [user.datafy-fs :as fs]
               :cljs [user.datafy-fs :as-alias fs])
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [user.util :refer [includes-str? pprint-str]])
  #?(:cljs (:require-macros user.demo-7-explorer)))

; all explorer bindings and p/fns must be called from server
(p/def indent-level (p/client 0))
(p/defn Indent [Xs]
  (p/client
    (binding [indent-level (inc indent-level)]
      ; top-down render pass
      (p/server (p/for [X Xs]
                  (X.))))))

#?(:cljs (defn format-value [k v]
           (case k
             ::fs/lastModifiedTime (.toLocaleDateString v)
             (str v))))

(p/defn Render-row [m]
  (p/client
    (dom/tr
      (dom/td {:style {:padding-left (-> indent-level (* 10) (str "px"))}}
        (p/server (::fs/name m)))
      (p/server
        (do (p/for [[k v] m]
              (p/client (dom/td (format-value k v))))
            nil)))))

(p/def Tree)
(p/defn Explorer [h]
  (assert (.isDirectory h) (str "file handle: " (.getName h) " is not a directory"))
  (p/client
    (let [!search (atom "") search (p/watch !search)]
      (dom/div {:class "photon-demo-explorer"}
        (dom/div {:class "title"} (p/server (.getAbsolutePath h)))
        (ui/input {::dom/placeholder "Search files by name" ::dom/type "search"
                   ::ui/input-event (p/fn [e] (reset! !search (.. e -target -value)))})
        (dom/hr)
        (dom/table
          (p/server
            (binding [Tree (p/fn [m] ; recur via binding until Photon gets proper recursion syntax
                             (->> (p/for [x (datafy (nav m ::fs/children (::fs/children m)))]
                                    (let [m (datafy x)]
                                      (if (::fs/children m)
                                        (let [Xs (Tree. m)] ; prewalk
                                          (when (seq Xs)
                                            ; don't render bottom up; thunk it for later top-down pass
                                            (p/fn []
                                              (Render-row. m) ; omit folder row if no descendents matched filter
                                              (Indent. Xs))))
                                        (when (includes-str? (::fs/name m) search) ; return nil to remove level
                                          (p/fn [] (Render-row. m))))))
                                  (remove nil?)))]

              ;(Render-row. h) -- never render target folder
              (if-let [Xs (seq (Tree. (datafy h)))]
                (Indent. Xs)
                (p/client (dom/div "no results matched"))))))))))

(def !target #?(:cljs (atom "src") :clj nil)) (p/def target (p/client (p/watch !target)))

(p/defn App []
  (dom/div
    (dom/h1 "Folder Explorer")
    (dom/link {:rel :stylesheet, :href "user_demo_explorer.css"})
    #_(dom/div "Folder: " (ui/button {::ui/click-event (p/fn [e] (reset! !target "node_modules"))} "node_modules"))
    (p/server (Explorer. (clojure.java.io/file (p/client target))))))

; Improvements
; Native search
; reduce amount of dom nodes (avoid p/hook)
;   grid instead of table
;   lazy folding/unfolding directories (no need for pagination)
