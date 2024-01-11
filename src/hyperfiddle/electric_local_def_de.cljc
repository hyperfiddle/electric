(ns hyperfiddle.electric-local-def-de
  (:refer-clojure :exclude [def defn])
  #?(:cljs (:require-macros hyperfiddle.electric-local-def-de))
  (:require [clojure.core :as cc]
            [contrib.cljs-target]
            [hyperfiddle.electric.impl.lang-de :as lang]))

(cc/defn ->local-config [env]
  (let [p (if (:js-globals env) :cljs :clj)] {::lang/peers {:client p, :server p}, ::lang/current :server}))

(cc/defn ->single-peer-config [env]
  (let [p (if (and (:js-globals env) (contrib.cljs-target/do-nodejs true)) :client :server)]
    {::lang/peers {p (if (:js-globals env) :cljs :clj)}, ::lang/current p, ::lang/me p}))

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
