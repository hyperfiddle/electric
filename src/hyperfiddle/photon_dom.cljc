(ns hyperfiddle.photon-dom
  (:refer-clojure :exclude [for])
  (:require [hyperfiddle.photon :refer [defnode for]]
            [missionary.core :as m])
  #?(:cljs (:require-macros
             [hyperfiddle.photon-dom :refer
              [mount! element-with-children fragment div span h1 table thead tbody tr td form field select option]])))

(defn append-children [parent items] (reduce #?(:cljs #(doto %1 (.appendChild %2))) parent items))
(defn remove-children [parent items] (reduce #?(:cljs #(doto %1 (.removeChild %2))) parent items))

(defn mount* [parent items]
  (m/observe
    (fn [!]
      (! (append-children parent items))
      (fn []
        (remove-children parent items)))))

(defn create-element! [type]
  #?(:cljs (.createElement js/document type)))

(defn set-fragment! [e f]
  ;; TODO
  )

(defn set-text-content! [e t])

(defn set-attribute! [e k v]
  #?(:cljs (.setAttribute e k v)))

(defnode mount! [e c]
  ~(mount* e c))

(defnode element-with-children [type children]
  (let [e (create-element! (name type))]
    (for [c children] (mount! e c)) e))

(defnode fragment [& body])
(defnode div [& body])

(defnode span [& children]
  (element-with-children :span children))

(defnode h1 [& children]
  (element-with-children :h1 children))

(defnode input [x]
  (doto (create-element! "input")
    (set-attribute! "type" "text")
    (set-attribute! "value" x)))

(defnode table [& children]
  (element-with-children :table children))

(defnode thead [& children]
  (element-with-children :thead children))

(defnode tbody [& children]
  (element-with-children :tbody children))

(defnode tr [& children]
  (element-with-children :tr children))

(defnode td [& children]
  (element-with-children :td children))

;; TODO
(defnode radio [value options]
  (for [[id text] options]
    [(doto (create-element! "input")
       (set-attribute! "type" "radio"))
     (doto (create-element! "label")
       (set-attribute! "for" id)
       (set-text-content! text))]))

(defnode form [& body])
(defnode field [& body])

(defnode select [selected options]
  (doto (create-element! "select")
    (mount! (for [[k v] options]
              (doto (create-element! "option")
                (set-attribute! "value" k)
                (set-text-content! v))))
    (set-attribute! "selected" selected)))

(defnode option [& body])
