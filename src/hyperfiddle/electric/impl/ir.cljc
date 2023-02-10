(ns hyperfiddle.electric.impl.ir
  (:refer-clojure :exclude [apply eval]))

(defn literal [x]
  {::op ::literal
   ::value x})

(defn sub [idx]
  {::op ::sub
   ::index idx})

(defn pub [init inst]
  {::op ::pub
   ::init init
   ::inst inst})

(defn constant [init]
  {::op ::constant
   ::init init})

(defn target [deps]
  {::op ::target
   ::deps deps})

(defn apply [f & args]
  {::op ::apply
   ::fn f
   ::args args})

(defn global [k]
  {::op   ::global
   ::name k})

(defn variable [init]
  {::op ::variable
   ::init init})

(def source
  {::op ::source})

(defn input [deps]
  {::op ::input
   ::deps deps})

(defn output [init]
  {::op   ::output
   ::init init})

(defn inject [slot]
  {::op ::def
   ::slot slot})

(defn eval [form]
  {::op ::eval
   ::form form})

(defn node [slot]
  {::op   ::node
   ::slot slot})

(defn bind [slot index inst]
  {::op ::bind
   ::slot slot
   ::index index
   ::inst inst})

(defn lift [inst]
  {::op ::lift
   ::init inst})

(defn do [deps inst]
  {::op ::do
   ::deps deps
   ::inst inst})

(def nop
  {::op ::nop})