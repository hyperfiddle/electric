(ns hyperfiddle.client.ui.sugar
  (:refer-clojure :exclude [time meta]) ; replaced by html tags
  (:require [clojure.edn :as edn]
            #?(:clj [clojure.java.io :as io])
            [clojure.walk :as walk]
            [missionary.core :as m]
            [hyperfiddle.client.ui :as ui]
            [hfdl.lang :refer [#?(:clj vars)]])
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

(defn html* [body]
  (if (empty? body)
    nil
    (let [[tag props & children] body]
      (if (keyword? tag)
        (if (= :text tag)
          `(hyperfiddle.client.ui/text ~props)
          `(hyperfiddle.client.ui/tag ~tag ~props ~@(map html* children)))
        `(~tag ~props ~@(map html* children))))))

(defmacro html [body] (html* body))

(defmacro gen-tags! []
  (let [tags (:tags (edn/read-string (slurp (io/file "./resources/html_spec.edn"))))]
    (cons 'do (for [[tag data] tags] `(def ~(symbol tag) ~(:doc data) (partial ui/tag ~tag))))))

(gen-tags!)

#?(:clj
   (defmacro export-tags! []
     (let [tags (keys (:tags (edn/read-string (slurp (io/file "./resources/html_spec.edn")))))]
       `(vars ~@(map symbol tags)))))


(def exports (export-tags!))
