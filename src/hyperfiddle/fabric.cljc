(ns hyperfiddle.fabric
  (:require
    [minitest :refer [tests]])
  #?(:clj
     (:import
       haxe.lang.VarArgsBase
       haxe.root.Array
       hyperfiddle.Origin)))


(defmulti clj->hx (fn [any]
                    (cond
                      (ifn? any) #?(:clj clojure.lang.IFn :cljs ::fn)
                      :else (type any))))

(defmethod clj->hx #?(:clj clojure.lang.IFn :cljs ::fn) [cljf]
  #?(:cljs (fn [& args] (apply cljf (seq args)))
     :clj  (proxy [haxe.lang.VarArgsBase] [-1 -1]           ; constructor params
             (__hx_invokeDynamic [args]
               (apply cljf (seq args))))))

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

(defn input [& [lifecycle-fn]] (Origin/input lifecycle-fn))

(defn on [>v f] (Origin/on >v (clj->hx f)))

(defn put [>a v] (.put >a v))

(tests
  (def >a (input)) => nil
  (put >a 1) => nil                                         ; no listener
  (-> >a .-node .-val) => 1                                 ; last value retained
  (def seen (atom {})) => #'hyperfiddle.fabric/seen
  (defn cap [k v]
    (println 'seen k v)
    (swap! seen update k (fnil conj []) v))                 ; tests accumulate state
  => #'hyperfiddle.fabric/cap

  (on >a (partial cap :a))
  => nil
  (put >a 2)
  => nil
  (put >a 3)
  => nil
  (:a @seen)
  => [2 3]
  )

(defn fmap [f & >as] (Origin/apply (hx-array >as) (clj->hx f)))

(tests
  (do
    (def >b (input))
    (def >bm (fmap inc >b))
    (on >bm (partial cap :b))
    (put >b 50)) => nil                                     ; second cap
  (:b @seen) => [51]

  (do
    (def >c (fmap vector >a >bm))
    (on >c (partial cap :c))
    (put >a 100)) => nil
  (:c @seen)
  => nil                                                    ; no :c, awaiting :b
  (put >b 100) => nil
  @seen
  => [[101 100]]
  )
