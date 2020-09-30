(ns hyperfiddle.fabric
  (:require
    [minitest :refer [tests]])
  #?(:clj
     (:import
       haxe.lang.VarArgsBase
       haxe.root.Array
       hyperfiddle.hx.Origin)))


(defmulti clj->hx (fn [any]
                    (cond
                      (ifn? any) #?(:clj clojure.lang.IFn :cljs ::fn)
                      :else (type any))))

(defmethod clj->hx #?(:clj clojure.lang.IFn :cljs ::fn) [cljf]
  #?(:cljs (fn [& args] (apply cljf (seq args)))
     :clj  (proxy [haxe.lang.VarArgsBase] [-1 -1]           ; constructor params
             (__hx_invokeDynamic [args]
               (try
                 (apply cljf (seq args))
                 (catch Exception e (.printStackTrace e)))))))

(defmethod clj->hx :default [any] any)

(tests
  (clj->hx 1) => 1
  (ifn? identity) => true
  (ifn? (clj->hx identity)) => false
  )

(set! (. Origin -onError) (clj->hx #(throw %)))

(defn hx-array [seq]
  #?(:cljs (object-array seq)
     :clj  (let [o (haxe.root.Array.)]
             (doseq [s seq]
               (.push o s))
             o)))

(defn input [& [?f]] (Origin/input ?f))

(defn on [>v f]
  #?(:clj (Origin/on >v (clj->hx f))))

(defn put [>a v] (.put >a v))

(tests
  (def >a (input)) => nil
  (-> >a .-node .-val) => nil                               ; peek node state
  (put >a 1) => nil                                         ; no listener
  (-> >a .-node .-val) => 1                                 ; last value retained
  (def seen (atom {}))
  => nil
  (defn cap [k v] (println 'seen k v)
    (swap! seen update k (fnil conj '()) v))
  => nil

  (on >a (partial cap :a))
  => nil
  (put >a 2)
  => nil
  (put >a 3)
  => nil
  @seen
  => {:a (3 2)}
  )

(defn fmap [f & >as]
  #?(:clj
     (Origin/apply (hx-array >as) (clj->hx f))))

(comment
  (def >b (input))
  (def >bm (fmap inc >b))
  (on >bm (partial cap :b))
  (put >b 50)                                               ; second cap
  @seen
  => {:a (3 2), :b (51)}

  (def >c (fmap vector >a >bm))
  (on >c (partial cap :c))
  (put >a 100)
  @seen
  => {:a (100 3 2), :b (51)}                                ; no :c, awaiting :b
  (put >b 100)
  @seen
  => {:a (100 3 2), :b (101 51), :c ([100 101])}            ; :c
  )
