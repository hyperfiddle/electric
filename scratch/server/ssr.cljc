(ns hyperfiddle.server.ssr
  (:require [hfdl.impl.switch :refer [switch]]
            [hfdl.lang :refer [vars]]
            [missionary.core :as m]))

;; TODO use org.w3c.dom instead, or just drop this aspect of the UI.
(defprotocol ITextNode
  (-get-text-content [this])
  (-set-text-content! [this text]))

(deftype TextNode [^:volatile-mutable text]
  ITextNode
  (-get-text-content [this] text)
  (-set-text-content! [this text'] (set! text text') this))

#?(:clj
   (defmethod print-method TextNode [this w]
     (print-simple (pr-str [:text (-get-text-content this)]) w)))

;; TODO use org.w3c.dom instead, or just drop this aspect of the UI.
(defprotocol INode
  (-get-children [this])
  (-append-child! [this child])
  (-remove-child! [this child]))

(deftype Node [tag ^:volatile-mutable children]
  INode
  (-get-children [this] children)
  (-append-child! [this child] (set! children (conj children child)) this)
  (-remove-child! [this child] (set! children (disj children child)) this))

#?(:clj
   (defmethod print-method Node [this w]
     (print-simple (pr-str (into [(.tag this)] (-get-children this))) w)))

(defn create-text-node [initial-value] (TextNode. (str initial-value)))
(defn create-tag-node [tag] (Node. tag #{}))

(defn by-id [id] (Node. id #{}))

(defn set-text-content! [elem text] (-set-text-content! elem text))

(defn text [>text]
  (let [elem (create-text-node "")]
    (m/stream! (m/latest #(set-text-content! elem %) >text))
    (m/ap elem)))

(defn append-childs [parent items] (reduce #(-append-child! %1 %2) parent items))
(defn remove-childs [parent items] (reduce #(-remove-child! %1 %2) parent items))

(defn mount [parent items]
  (m/observe
   (fn [!]
     (! (append-childs parent items))
     (fn []
       (remove-childs parent items)))))

(defn tag [elem _>props & >childs] ; props not supported on SSR yet
  (let [elem (create-tag-node elem)]
    (when (seq (filter identity >childs))
      (m/stream! (switch (apply m/latest #(mount elem %&) >childs))))
    (m/ap elem)))

(defn append-child! [parent >child]
  (m/stream! (switch (m/latest #(mount parent [%]) >child)))
  parent)

(defn mount-component-at-node! [id >component]
  (append-child! (by-id id) >component))

(def exports (vars by-id text tag mount-component-at-node!))

