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
                      (ifn? any) ::fn
                      () (type any))))

(defmethod clj->hx ::fn [f]
  #?(:cljs (fn [& args] (apply f (seq args)))
     :clj  (proxy [haxe.lang.VarArgsBase] [-1 -1]           ; constructor params
             (__hx_invokeDynamic [args]
               (apply f (seq args))))))

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

(defn from-hx-array [hx-arr]
  (let [it (.iterator hx-arr)]
    (iterator-seq
      (reify #?(:clj java.util.Iterator)
        (hasNext [this] (.hasNext it))
        (next [this] (.next it))))))

(defn input [& [lifecycle-fn]] (Origin/input lifecycle-fn))

(defn on [>v f] (Origin/on >v (clj->hx f)))

(defn put [>a v] (.put >a v))

(tests
  (do
    (def >a (input))
    (put >a 1)                                              ; no listener yet, will not propagate
    (-> >a .-node .-val)                                    ; last value retained
    (def seen (atom nil))
    ;(add-watch seen 'k (fn [k r o n] (println 'seen n)))
    (on >a #(reset! seen %))
    @seen)
  => nil
  (do (put >a 2) @seen) => 2
  (do (put >a 3) @seen) => 3)

(defn fmap [f & >as]
  (Origin/apply (hx-array >as)
    (clj->hx (fn [hx-args]
               (apply f (from-hx-array hx-args))))))

(tests
  ; fmap a stream
  (do
    (def s (atom nil))
    (def >b (input))
    (def >b' (fmap inc >b))
    (on >b' #(reset! s %))
    (put >b 50)
    @s)
  => 51

  ; join two streams
  (do
    (def s (atom nil))
    (def >a (input))
    (def >b (input))
    (def >c (fmap vector >a >b))
    (on >c #(reset! s %))
    (put >a :a)
    @s) => nil                                              ; awaiting b
  (do (put >b :b) @s) => [:a :b]                            ; now b

  ; join N streams
  (do
    (def N 100)
    (def s (atom nil))
    (def >ss (take N (repeatedly input)))
    (on (apply fmap vector >ss) #(reset! s %))
    (doseq [>s >ss] (put >s ::foo))
    (count @s)) => N
  )

(defn fapply [>f & >as] (apply fmap #(apply % %&) >f >as))

(tests
  (do
    (def s (atom nil))
    (def >f (input))
    (def >a (input))
    (def >b (input))
    (on (fapply >f >a >b) #(reset! s %))
    (put >f +)
    (put >a 1)
    (put >b 2)
    @s) => 3
  (do (put >f -) @s) => -1
  )
