(ns user.demo-7-explorer
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [user.util :refer [includes-str?]])
  #?(:cljs (:require-macros user.demo-7-explorer)))

; wrap java interop (hack)
(defn file-is-dir [handle] #?(:clj (.isDirectory handle)))
(defn file-is-file [handle] #?(:clj (.isFile handle)))
(defn file-list-files [handle] #?(:clj (seq (.listFiles handle))))
(defn file-get-name [handle] #?(:clj (.getName handle)))

(comment
  (def h (clojure.java.io/file "node_modules/"))
  (file-get-name h)
  (file-is-dir h)
  (file-list-files h)
  (for [x (take 5 (file-list-files h))]
    (file-get-name x))
  )

(p/def FooRecur)
(p/defn Foo [handle s]
  ; hacked recursion workaround (Photon compiler hasn't implemented recursion yet)
  (binding [FooRecur (p/fn [x s]                            ; only call from server due to https://github.com/hyperfiddle/photon/issues/12
                       (p/server
                         (cond
                           (file-is-dir x)
                           (let [fname (file-get-name x)
                                 xs (file-list-files x)]
                             (p/client (dom/li (dom/text fname))
                                       (dom/ul (p/server (p/for [x (take 50 xs)] ; limit length, fixme
                                                           (FooRecur. x s))))))
                           (file-is-file x)
                           (let [fname (file-get-name x)]
                             (when (includes-str? fname s)
                               (p/client (dom/li (dom/text fname))))))))]
    (p/client (dom/ul (p/server (FooRecur. handle s))))))

(def !r #?(:cljs (atom "") :clj nil))
(p/def r (p/client (p/watch !r)))

(p/defn App []
  (dom/div
    (dom/h1 (dom/text "Explorer"))
    (ui/input {::dom/placeholder "Filter" ::dom/type "search" :style {:width "20rem"}
               ::ui/input-event  (p/fn [e] (reset! !r (:value dom/node)))})
    (p/server (Foo. (clojure.java.io/file "src") (p/client r)))))

(comment
  !r
  (reset! !r "LICENSE")
  (reset! !r "")
  )