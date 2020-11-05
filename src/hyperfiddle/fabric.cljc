(ns hyperfiddle.fabric
  (:require
    [minitest :refer [tests]]
    [hyperfiddle.via :refer [via Do-via]]
    [promesa.core :as p])
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

(set! (. Origin -onError) (clj->hx #(throw %)))

(defn input [& [lifecycle-fn]] (Origin/input lifecycle-fn))

(defn on [>a f] (Origin/on >a (clj->hx f)))

(defn put [>a v] (.put >a v) nil)

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

(defn- fmap-async [f & >as]
  (Origin/applyAsync (clj->hx >as)
                     (clj->hx (fn [hx-args, hx-cont]
                                (let [cont (hx->clj hx-cont)]
                                  (-> (p/future (apply f (hx->clj hx-args)))
                                      (p/then (fn [val] (cont true val)))
                                      (p/catch (fn [err] (cont false err)))))))))

(defn fapply [>f & >as] (apply fmap #(apply % %&) >f >as))

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

  ; fmap async
  (do
    (def s (atom nil))
    (def >b (input))
    (def >b' (fmap-async (fn [x]
                           (Thread/sleep 500)
                           (inc x))
                         >b))
    (on >b' #(reset! s %))
    (put >b 50)
    @s)
  => nil

  (do (Thread/sleep 1000) @s) => 51

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

(tests
  ; applicative interpreter

  (do
    (deftype Fabric []
      Do-via
      (resolver-for [R]
        {:Do.fmap   (fn [f mv] (fmap f mv))               ; varargs?
         :Do.pure   (fn [v] (doto (input) (.put v)))      ; does the effect happen to soon?
         :Do.fapply (fn [af & avs] (apply fapply af avs))
         :Do.bind   (fn [mv mf] (assert false))
         }))

    (def >a (input))
    (def >z (via (->Fabric)
              (let [>b (inc ~>a)
                    >c (dec ~>a)]
                (vector ~>b ~>c :x))))

    (def s (atom []))
    (on >z #(swap! s conj %))
    (->> (iterate inc 0) (map #(put >a %)) (take 3) doall)
    @s) => [[1 -1 :x] [2 0 :x] [3 1 :x]]

  )
