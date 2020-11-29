(ns hyperfiddle.fabric
  {:clj-kondo/config '{:lint-as {hyperfiddle.fabric/defnode clojure.core/def}}}
  (:require
    [minitest :refer [tests]]
    [hyperfiddle.via :refer [via Do-via]]
    [promesa.core :as p]
    [hyperfiddle.hxclj :refer [hx->clj clj->hx]])
  #?(:clj
     (:import
       hyperfiddle.Flow
       hyperfiddle.Origin
       )))

(defn compute-executor ; default
  [& [_id f & args]]
  (apply (hx->clj f) args))

(defn tracing-executor [writef]
  (fn [& [_id f & args]]
    (let [result (apply (hx->clj f) args)]
      (writef [_id result])
      result)))

(defn cache-or-compute-executor [cachef]
  (fn [& [id f & args]]
    (or (get (cachef) id)
        (apply compute-executor id f args))))

(defn set-executor! [f]
  (set! (. Origin -executor) (clj->hx f)))

(set-executor! compute-executor)

(set! (. Origin -onError) (clj->hx #(throw %)))

(def ^:private ^:dynamic *node-name* nil)

(defmacro defnode [name & body]
  `(binding [*node-name* '~name] ;; BROKEN: might create many nodes with the
                                 ;; same name. Instead use `(doto ~@body (.setName name))`
     (def ~name ~@body)))

(defn input [& [on off]]
  (Origin/input *node-name*
                (when on
                  (clj->hx (fn []
                             (on)
                             (when off (clj->hx off)))))))

(defn on [>a f] (Origin/on *node-name* >a (clj->hx f)))

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
  (declare >a)
  !! (def >a (input))
  !! (put >a 1)                                             ; no listener yet, will not propagate
  (type (-> >a .-node .-val)) => hyperfiddle.Maybe          ; last value retained
  !! (def s (cap >a #_println))
  @s => nil
  !! (put >a 2) @s => 2
  !! (put >a 3) @s => 3)

(defn fmap [f & >as]
  (Origin/apply *node-name* (clj->hx >as)
    (clj->hx (fn [hx-args]
               (apply f (hx->clj hx-args))))))

(defn fmap-async [f & >as]
  (Origin/applyAsync *node-name*
                     (clj->hx >as)
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
  (tests

    !! (def >a (input))
    !! (def >b (input))
    !! (def >c (fmap vector >a >b))
    !! (def s (cap >c))
    !! (put >a :a)
    @s => nil                                               ; awaiting b
    !! (put >b :b)
    @s => [:a :b]                                           ; now b
    )

  ; join N streams
  (do
    (def N 100)
    (def >ss (take N (repeatedly input)))
    (def >z (apply fmap vector >ss))
    (def s (cap >z))
    (doseq [>s >ss] (put >s ::foo))
    (count @s)) => N
  )

(defn pure [c] (Origin/pure *node-name* c))

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
  ;!! (off >out)
  @s => [:on #_:off]

  ;!! (def s2 (cap (fmap identity >a)))
  ;!! (put >a 1)
  ;@s2 => 1

  )

(tests
  "diamond"

  !!                                                        ; broken
  (do
    (def >a (input))
    (def >b (fmap inc >a))
    (def >z (fmap vector >a >b))
    (def s (cap >z))
    (put >a 1)
    @s)
  => [1 2]

  !!
  (do
    (def >a (input))
    (def s1 (cap (fmap vector >a (fmap inc >a))))
    (def s2 (cap (fmap vector (fmap inc >a) >a)))
    (put >a 1)
    [@s1 @s2])
  => [[1 2] [2 1]]

  )

(defn bind [>a f] (Origin/bind *node-name* >a (clj->hx f)))

(tests
  @(cap (bind (pure 1) (fn [a] (pure a))))
  => 1

  !! (def >a (input #_#(print "a on")))
  !! (def >b (input #_#(print "b on")))
  !! (def >control (input #_#(print "control on")))
  !! (def >cross (bind >control (fn [c] (case c :a >a :b >b))))
  !! (def >x (fmap vector >a >b >cross))
  !! (def s (history >x #_print))
  !! (do (put >control :b) (put >a 1) (put >b 2))
  @s => [[1 2 2]]
  
  @(cap (fmap identity (pure 1))) => 1
  @(cap (bind (pure 1) pure)) => 1
  @(cap (bind (fmap identity (pure 1)) pure)) => 1
  
  )

(tests
  "error recovery (don't corrupt flow state on error)"
  !! (def >a (input))
  !! (def s (cap (fmap (fn [a] (assert false)) >a)))
  !! (put >a 1)        ; why is exception not re-thrown? todo
  @s => nil
  @(cap (pure 1)) => 1 ; recovered

  "error recovery from bind type error"
  (class (try @(cap (bindR (pure 1) identity)) (catch Throwable e e)))
  => nil #_haxe.Exception
  @(cap (pure 1)) => 1 ; recovered

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
  (def >b (bind >a (fn [a] (if a >x >y))))
  (def s (cap >b))
  (put >a true)
  (put >x 1)
  @s => 1

  ;(bindR >a (fn [a] (if a (put >x a) (put >y a))))


  (def >b
    (bind >a (fn [a] (case a
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
