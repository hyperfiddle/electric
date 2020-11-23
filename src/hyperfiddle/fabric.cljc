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

(def Graph (new hyperfiddle.Origin))

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

;; (set! (. Graph -onError) (clj->hx #(throw %)))

(defn input [& [on-fn]]
  (.input Graph (when on-fn
                  (clj->hx (fn []
                             (when-let [off-fn (on-fn)]
                               (clj->hx off-fn)))))))

(defn on [>a f] (.on Graph >a (clj->hx f)))

(defn off [output] (.off output))

(defn put [>a v] (.put >a v) nil)

(defn cap
  "Stateful stream terminator that remembers the most recent value. But, what
  are the pros and cons of this compared to exposing the equivalent private
    node state?"
  [>x]
  (let [s (atom nil)]
    (on >x #(reset! s %))
    s))

(tests
  !! (do
    (def >a (input))
    (put >a 1)                                              ; no listener yet, will not propagate
    (-> >a .-node .-val)                                    ; last value retained
    (def s (cap >a)))
  @s => nil
  !! (put >a 2) @s => 2
  !! (put >a 3) @s => 3)

(defn fmap [f & >as]
  (.apply Graph (clj->hx >as)
    (clj->hx (fn [hx-args]
               (apply f (hx->clj hx-args))))))

(defn fmap-async [f & >as]
  (.applyAsync Graph (clj->hx >as)
                     (clj->hx (fn [hx-args, hx-reject, hx-resolve]
                                (let [reject (hx->clj hx-reject)]
                                  (try
                                    (-> (apply f (hx->clj hx-args))
                                        (p/then (hx->clj hx-resolve))
                                        (p/catch reject))
                                    (catch Throwable t
                                      (reject t))))))))

(tests
  ; fmap a stream
  (do
    (def >b (input))
    (def >b' (fmap inc >b))
    (def s (cap >b'))
    (put >b 50)
    @s)
  => 51

  ; fmap async
  (do
    (def >b (input))
    (def >b' (fmap-async (fn [x]
                           (p/future
                             (Thread/sleep 1)
                             (inc x)))
                         >b))
    (def s (cap >b'))
    (put >b 50)
    @s)
  => nil
  !! (Thread/sleep 10)
  @s => 51

  ; join two streams
  (do
    (def >a (input))
    (def >b (input))
    (def >c (fmap vector >a >b))
    (def s (cap >c))
    (put >a :a)
    @s) => nil                                              ; awaiting b
  (do (put >b :b) @s) => [:a :b]                            ; now b

  ; join N streams
  (do
    (def N 100)
    (def >ss (take N (repeatedly input)))
    (def s (cap (apply fmap vector >ss)))
    (doseq [>s >ss] (put >s ::foo))
    (count @s)) => N
  )

(defn pure [c] (.pure Graph c))

(tests
  @(cap (pure 1)) => 1
  @(cap (fmap inc (pure 1))) => 2

  (do
    (def >ui (input))
    (def >a (pure 1))
    (def >b (fmap inc >a))                               ; View with current state even if no listeners
    (def >c (fmap vector >b >ui))
    (def s (cap >c))
    (put >ui "ui")
    @s) => [2 "ui"]

  @(cap (fmap vector (pure 1) (pure 2))) => [1 2]

  (do
    (def >f (input))
    (def >a (input))
    (def s (cap (fmap #(apply % %&) >f >a >a)))
    (put >f +)
    (put >a 1)
    @s) => 2
  !! (put >f -) @s => 0

  @(cap (fmap #(apply % %&) (pure +) (pure 1) (pure 1))) => 2
  @(cap (let [>C (pure 1)] (fmap #(apply % %&) (pure +) >C >C))) => 2
  )

(defn fapply "Provided for completeness, prefer varadic fmap"
  [>f & >as]
  (apply fmap #(apply % %&) >f >as))

(tests
  @(cap (fapply (pure +) (pure 1) (pure 2))) => 3
  )

(defn history [>x]
  (let [s (atom [])]
    (on >x #(swap! s conj %))
    s))

(tests
  ; applicative interpreter

  (do
    (deftype Fabric []
      Do-via
      (resolver-for [R]
        {:Do.fmap   (fn [f mv] (fmap f mv))               ; varargs?
         :Do.pure   (fn [v] (doto (input) (.put v)))      ; does the effect happen to soon?
         :Do.fapply (fn [af & avs] (apply fmap #(apply % %&) af avs))
         :Do.bind   (fn [mv mf] (assert false))
         }))

    (def >a (input))
    (def >z (via (->Fabric)
              (let [>b (inc ~>a)
                    >c (dec ~>a)]
                (vector ~>b ~>c :x))))

    (def s (history >z))

    (->> (iterate inc 0) (map #(put >a %)) (take 3) doall)
    @s) => [[1 -1 :x] [2 0 :x] [3 1 :x]]

  )
