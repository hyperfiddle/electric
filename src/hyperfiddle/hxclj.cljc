(ns hyperfiddle.hxclj
  (:require [minitest :refer [tests]])
  #?(:clj
     (:import
       haxe.lang.VarArgsBase
       haxe.root.Array)))


(defn -primary-predicate [v]                                ; this is really bad
  (cond                                                     ; order matters, they overlap
    ;(nil? v) nil?
    ;(keyword? v) keyword?
    ;(symbol? v) symbol?
    (seqable? v) seqable?
    ;(map? v) map?
    ;(set? v) set?
    ;(sequential? v) sequential?
    (fn? v) fn?
    () :default))

(defmulti clj->hx -primary-predicate)
(defmulti hx->clj type)                                     ; e.g. haxe.root.Array

(defmethod clj->hx :default [v] v)
;(defmethod hx->clj :default [v] v)

(defmethod clj->hx fn? [f]
  #?(:cljs (fn [& args] (apply f (seq args)))
     :clj  (proxy [haxe.lang.VarArgsBase] [-1 -1]           ; constructor params
             (__hx_invokeDynamic [args]
               (apply f (seq args))))))

(defmethod clj->hx seqable? [xs]
  #?(:cljs (object-array xs)
     :clj  (let [o (haxe.root.Array.)]
             (doseq [s xs]                                  ; (seq xs) internally
               (.push o s))
             o)))

(tests
  (clj->hx 1) => 1
  (type (clj->hx (seq '(a)))) => haxe.root.Array
  (type (clj->hx ['a])) => haxe.root.Array
  (isa? (class (clj->hx identity)) haxe.lang.Function) => true
  (isa? (class (clj->hx identity)) haxe.lang.VarArgsBase) => true
  #_(bases (class (clj->hx identity)))
  )

(defmethod hx->clj haxe.root.Array [v!]
  (let [it (.iterator v!)]
    (iterator-seq
      (reify #?(:clj java.util.Iterator)
        (hasNext [this] (.hasNext it))
        (next [this] (.next it))))))

(defmethod hx->clj haxe.lang.Function [hxf]
  (fn hx-call-proxy [& args]
    (.__hx_invokeDynamic hxf (into-array Object args))))

(tests
  (hx->clj (clj->hx '(a))) => '(a)
  (hx->clj (clj->hx [:a])) => '(:a)                         ; !
  )