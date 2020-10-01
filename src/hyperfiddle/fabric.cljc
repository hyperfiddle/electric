(ns hyperfiddle.fabric
  (:require
    [minitest :refer [tests]])
  #?(:clj
     (:import
       haxe.lang.VarArgsBase
       haxe.root.Array
       hyperfiddle.Origin)))


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
  ;(extends? haxe.lang.Function (clj->hx identity)) ; how to test this?
  )

(defmethod hx->clj haxe.root.Array [v!]
  (let [it (.iterator v!)]
    (iterator-seq
      (reify #?(:clj java.util.Iterator)
        (hasNext [this] (.hasNext it))
        (next [this] (.next it))))))

(tests
  (hx->clj (clj->hx '(a))) => '(a)
  (hx->clj (clj->hx [:a])) => '(:a)                         ; !
  )

(set! (. Origin -onError) (clj->hx #(throw %)))

(defn input [& [lifecycle-fn]] (Origin/input lifecycle-fn))

(defn on [>a f] (Origin/on >a (clj->hx f)))

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
  (Origin/apply (clj->hx >as)
    (clj->hx (fn [hx-args]
               (apply f (hx->clj hx-args))))))

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
