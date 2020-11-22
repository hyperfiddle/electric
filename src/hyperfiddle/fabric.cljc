(ns hyperfiddle.fabric
  (:require
    [minitest :refer [tests]]
    [hyperfiddle.via :refer [via Do-via]]
    [promesa.core :as p])
  #?(:clj
     (:import
       haxe.lang.VarArgsBase
       haxe.root.Array
       hyperfiddle.Flow
       hyperfiddle.Origin
       )))


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

(defn input [& [on off]]
  (Origin/input
    (some-> on clj->hx)
    (some-> off clj->hx)))

(defn on [>a f] (Origin/on >a (clj->hx f)))

(defn off [output] (.off output))

(defn put [>a v] (.put >a v) nil)

(defn cap
  "Stateful stream terminator that remembers the most recent value. But, what
  are the pros and cons of this compared to exposing the equivalent private
    node state?"
  [>x & [f]]
  (let [s (atom nil)]
    (on >x (fn [%]
             (reset! s %)
             (if f (f %))))
    s))

(defn history
  "like cap but with history"
  [>x & [f]]
  (let [s (atom [])]
    (on >x (fn [%]
             (swap! s conj %)
             (if f (f %))))
    s))

(tests
  !! (def >a (input))
  !! (put >a 1)                                             ; no listener yet, will not propagate
  (type (-> >a .-node .-val)) => hyperfiddle.Maybe          ; last value retained
  !! (def s (cap >a #_println))
  @s => nil
  !! (put >a 2) @s => 2
  !! (put >a 3) @s => 3)

(defn fmap [f & >as]
  (Origin/apply (clj->hx >as)
    (clj->hx (fn [hx-args]
               (apply f (hx->clj hx-args))))))

(defn fmap-async [f & >as]
  (Origin/applyAsync (clj->hx >as)
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

(defn pure [c] (Origin/pure c))

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

(tests
  "lifecycle"
  !! (def >s (input))
  !! (def s (history >s))

  !! (def >a (input #(put >s :on) #(put >s :off)))
  !! (def >out (on >a #()))
  !! (off >out)
  @s => [:on :off]

  ;!! (def s2 (cap (fmap identity >a)))
  ;!! (put >a 1)
  ;@s2 => 1

  )

;(tests
;  !! (def >a (input #(println "a on")))
;  !! (def >b (input #(println "b on")))
;  !! (def >control (input #(println "control on")))
;  !! (def >out1 (fmap identity >a))
;  !! (def >out2 (fmap vector >a >b))
;
;  !! (def s1 (history >out1 println))
;  !! (def s2 (history >out2 println))
;
;
;  !! (put >a 1)
;  @s1 => [1]
;  !! (put >b 2)
;  @s1 => [1]
;  @s2 => [1]
;
;  )

(defn bindR [>a f] (Origin/bind >a (clj->hx f)))

(tests
  ;@(cap (bindR (pure 1) (fn [a] (pure a))))
  ;=> 1

  ;@(cap (bindR (pure 1) identity))        ; breaks and leaves invalid state

  !! (def >a (input #(println "a on")))
  !! (def >b (input #(println "b on")))
  !! (def >control (input #(println "control on")))
  !! (def >cross (bindR >control (fn [c] (case c :a >a :b >b))))
  !! (def >x (fmap vector >a >b >cross))
  !! (def s (history >x println))

  !! (do (put >control :b) (put >a 1) (put >b 2))
  !! (println 'yo)
  !! (.. >cross -node -bridge -node (run (hyperfiddle.Origin/get)))

  @s => [[1 2 2]]

  )

(comment


  (.. >control -node -val)
  (.. >a -node -val)
  (.. >b -node -val)
  (.. >cross -node -val)
  ;(.. >cross -node -bridge -node -val)
  (.. >x -node -val)                                        ; state cleared

  #_(.. >b -node (run (hyperfiddle.Origin/get)))
  #_(.. >cross -node (run (hyperfiddle.Origin/get)))
  #_(.. >control -node (run (hyperfiddle.Origin/get)))
  (.. >cross -node -bridge -node (run (hyperfiddle.Origin/get)))
  (.. >x -node (run (hyperfiddle.Origin/get)))

  (.. >control -node active)
  (.. >a -node active)
  (.. >b -node active)
  (.. >x -node active)
  (.. >cross -node active)

  (.. >cross -node -val)
  (.. >cross -node on)
  (.. >cross -node to)
  (.. >cross -node active)
  (.. >cross -node joins)
  (.. >cross -node ok)
  (.. >cross -node -bridge -node)

  (.. >cross -node -bridge -node -on)
  (.. >cross -node -bridge -node -to)
  (.. >b -node -to)
  (.. >cross -node -bridge -node)
  (.. >x -node -val)




  )

(comment
  "laws"
  ; Left identity: return a >>= f ≡ f a
  ; Right identity: m >>= return ≡ m
  ; Associativity: (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
  )

(comment



  (def >x (input))
  (def >y (input))

  (def >a (input))
  (def >b (bindR >a (fn [a] (if a >x >y))))
  (def s (cap >b))
  (put >a true)
  (put >x 1)
  @s => 1

  ;(bindR >a (fn [a] (if a (put >x a) (put >y a))))


  (def >b
    (bindR >a (fn [a] (case a
                        1 >x
                        2 >y
                        3 >z
                        ...))))
  (fmap >b identity)



  ; 1: io/watch-file

  (defn watch-file [filename]
    (let [>x (df/input)]
      (watch-file path (fn [path] (df/put >x path)))
      >x))

  (def >config-filename (input))
  (def >config-contents (bind #(watch-file %) >config-filename))
  (history >config-contents)
  (put >config-filename "hyperfiddle.edn")

  ; 2: table/row ui
  (def >records (server ...))
  ;(fmap count >records)
  (bind (fn [records]
          (sequence
            (for [[k >r] (spread-rows records)]
              (render-row> >r))))
    >records)

  )


(comment

  ; bind can be used to switch between topologies
  ; or implement loops
  ; continuation

  (do
    (def >a (input))
    (def >b (bindR >a (fn [a] ...)))
    (def s (cap >b))
    (put >a 2)
    @s) => 2

  (do
    (def >a (input))
    #_(def >b (bindR >a (fn [a]
                          (doto (input) (put a)))))
    (def >b (bindR >a (fn []
                        (let [>b (input)]
                          (fn [a]
                            (put >b a))))))
    (def >b (bindR >a (fn [a]
                        (let [>b (if a (input) (input))]
                          (fn []
                            (put >b a))))))
    (def >b (bindR >a (fn [a]
                        (fn [put]
                          (let [>b (if a (input) (input))]
                            (put >b a))))))

    (def s (cap >b))
    (put >a 3)
    @s) => 3

  (do
    (def >a (input))
    (def >b (bind >a (fn [a]
                       (let [>b (input)
                             >c (fmap inc >b)]
                         (put >b a)
                         >c))))
    (def s (cap >b))
    (put >a 4)
    @s)
  => 3


  (def >c (Origin/bind (pure 1) (clj->hx identity)))
  @(cap >c)



  (.-node >a)
  (.-node >b)
  (.-node >c)

  )

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
