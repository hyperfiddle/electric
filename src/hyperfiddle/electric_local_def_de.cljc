(ns hyperfiddle.electric-local-def-de
  (:refer-clojure :exclude [compile])
  #?(:cljs (:require-macros hyperfiddle.electric-local-def-de))
  (:require [clojure.core :as cc]
            [contrib.cljs-target]
            #?(:clj [hyperfiddle.electric.impl.lang-de2 :as lang]
               :cljs [hyperfiddle.electric.impl.lang-de2 :as-alias lang])))

(defn ->local-config [env]
  (let [p (if (:js-globals env) :cljs :clj)] {::lang/peers {:client p, :server p}}))

(defn ->single-peer-config [env]
  (let [p (if (and (:js-globals env) (contrib.cljs-target/do-nodejs true)) :client :server)]
    {::lang/peers {p (if (:js-globals env) :cljs :clj)}, ::lang/me p}))

(def web-config {::lang/peers {:client :cljs, :server :clj}})

(defmacro compile-client [form]
  (let [env (merge &env (->local-config &env) {::lang/me :client, :ns (list 'quote (ns-name *ns*))})]
    `(:source (lang/compile '~form ~env))))
(defmacro compile-client-source-map [form]
  (let [env (merge &env (->local-config &env) {::lang/me :client})]
    `(:source-map (lang/compile '~form (assoc ~env ::lang/include-source-map true)))))
(defmacro compile-client-with-source-map [form]
  (let [env (merge &env (->local-config &env) {::lang/me :client})]
    `(lang/compile '~form (assoc ~env ::lang/include-source-map true))))
(defmacro compile-server [form]
  (let [env (merge &env (->local-config &env) {::lang/me :server})]
    `(:source (lang/compile '~form ~env))))

(defmacro compile
  ([nm form] `(compile ~nm identity ~form))
  ([nm env-fn form] `(lang/compile ~nm '~form (~env-fn '~(merge web-config (lang/normalize-env &env))))))

(defmacro compile-as-if-client
  ([nm form] `(compile-as-if-client ~nm identity ~form))
  ([nm env-fn form] `(lang/compile ~nm '~form (~env-fn '~(merge web-config (lang/->cljs-env))))))
