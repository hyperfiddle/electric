(ns hyperfiddle.client.examples.card
  "A custom wrapper for photon dom effects."
  (:require [devcards.core :as dc]
            #?(:cljs [devcards.util.utils :as utils :refer [html-env? define-react-class]])
            #?(:cljs [goog.object :as gobj])
            #?(:cljs [react :as react])
            #?(:cljs [react-dom :as react-dom]))
  #?(:cljs (:require-macros [hyperfiddle.client.examples.card :refer [dom-node]])))

#?(:cljs
   (define-react-class MutableComponent
     (constructor
      [props]
      (this-as this
        (set! (.-state this)
              #js {:unique_id (str (gensym 'devcards-dom-component-))})))
     (componentDidUpdate
      [this prevP prevS]
      (when (and (dc/get-props this :node_fn)
                 (not= (dc/get-props this :node_fn)
                       (gobj/get prevP "node_fn")))
        (dc/render-into-dom this)))
     (componentWillUnmount
      [this]
      (when-let [node (dc/ref->node this (dc/get-state this :unique_id))]
        (react-dom/unmountComponentAtNode node)))
     (componentDidMount [this] (dc/render-into-dom this))
     (render
      [this]
      (if (html-env?)
        (react/createElement "div"
                             #js { :className "com-rigsomelight-devcards-dom-node" :ref (dc/get-state this :unique_id)} nil)
        (react/createElement "div" nil nil)))))


(defn dom-node [node-fn]
   #?(:cljs (fn [data-atom owner]
              (react/createElement MutableComponent
                                   #js {:node_fn   node-fn
                                        :data_atom data-atom}))))

#_(defmacro dom-node [body]
  (when (utils/devcards-active?)
    `(dom-node* ~body)))
