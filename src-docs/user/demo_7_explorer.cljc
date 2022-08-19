(ns user.demo-7-explorer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [user.util :refer [includes-str?]])
  #?(:cljs (:require-macros user.demo-7-explorer)))

(comment
  (def h (clojure.java.io/file "node_modules/"))
  (.getName h)
  (.isDirectory h)
  (.listFiles h)
  (for [x (take 5 (.listFiles h))]
    (.getName x))
  )

(p/def FooRecur)
(p/defn Foo [handle s]
  ; hacked recursion workaround (Photon compiler hasn't implemented recursion yet)
  (binding [FooRecur (p/fn [x s]                            ; only call from server due to https://github.com/hyperfiddle/photon/issues/12
                       (p/server
                         (cond
                           (.isDirectory x)
                           (p/client (dom/li (p/server (.getName x)))
                                     (dom/ul (p/server (p/for [x (take 100 (.listFiles x))]
                                                         (FooRecur. x s)))))
                           (.isFile x)
                           (let [fname (.getName x)]
                             (when (includes-str? fname s)
                               (p/client (dom/li fname)))))))]
    (p/client (dom/ul (p/server (FooRecur. handle s))))))

(def !target #?(:cljs (atom "src") :clj nil)) (p/def target (p/client (p/watch !target)))
(def !search #?(:cljs (atom "") :clj nil)) (p/def search (p/client (p/watch !search)))

(p/defn App []
  (dom/div
    (dom/h1 "Folder Explorer")
    (dom/div "Folder: "
      (ui/button {::ui/click-event (p/fn [e] (reset! !target "src"))} "src")
      (ui/button {::ui/click-event (p/fn [e] (reset! !target "node_modules")) ::dom/disabled true} "node_modules (todo)"))
    (dom/p "Try typing \"compiler\"")
    (ui/input {::dom/placeholder "Search files by name" ::dom/type "search" :style {:width "40rem"}
               ::ui/input-event  (p/fn [e] (reset! !search (:value dom/node)))})
    (p/server (Foo. (clojure.java.io/file (p/client target)) (p/client search)))))

(comment
  !search
  (reset! !search "LICENSE")
  (reset! !search "")
  )
