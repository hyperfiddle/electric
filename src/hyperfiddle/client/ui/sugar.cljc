(ns hyperfiddle.client.ui.sugar
  (:refer-clojure :exclude [time meta]) ; replaced by html tags
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [missionary.core :as m]
            [hyperfiddle.client.ui :as ui]
            [hfdl.lang :refer [vars]])
  #?(:cljs (:require-macros [hyperfiddle.client.ui.sugar :refer [gen-tags! export-tags! html]])))

(defn hiccup? [x]
  (and (vector? x)
       (keyword? (first x))))

(defn props-setter [props keys & vals]
  (apply assoc props (interleave keys vals)))

(defn rewrite-props [props]
  (when props
    (let [{:keys [props keys vals]}
          (reduce-kv (fn [r k v]
                       (if (and (sequential? v)
                                (#{'cljs.core/unquote 'clojure.core/unquote} (first v)))
                         (-> (update r :keys conj k)
                             (update :vals conj (second v)))
                         (update r :props assoc k v)))
                     {:props {}
                      :keys  []
                      :vals  []}
                     props)]
      (if (empty? keys)
        `(m/ap ~props)
        `(m/latest (partial props-setter ~props ~keys) ~@vals)))))

(defmacro html [body]
  (walk/prewalk (fn [x]
                  (if (hiccup? x)
                    (let [[tag props & children] x
                          props?                 (map? props)
                          props'                 (if props? props nil)
                          children               (filter some? (if-not props? (cons props children) children))]
                      (if props?
                        `(tag ~tag ~(rewrite-props props') ~@children)
                        `(tag ~tag nil ~@children)))
                    x))
                body))

(defmacro gen-tags! []
  (let [tags (:tags (edn/read-string (slurp (io/file "./resources/html_spec.edn"))))]
    (cons 'do (for [[tag data] tags] `(def ~(symbol tag) ~(:doc data) (partial ui/tag ~tag))))))

(gen-tags!)

#?(:clj
   (defmacro export-tags! []
     (let [tags (keys (:tags (edn/read-string (slurp (io/file "./resources/html_spec.edn")))))]
       `(vars ~@(map symbol tags)))))


(def exports (export-tags!))
