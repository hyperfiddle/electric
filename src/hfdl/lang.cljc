(ns hfdl.lang
  (:refer-clojure :exclude [def binding fn for eval defn])
  (:require [clojure.core :as cc]
            [hfdl.impl.compiler :as c]
            [hfdl.impl.runtime :as r]
            [hfdl.impl.util :as u]
            [hfdl.impl.rfor :refer [rfor]]
            [hfdl.impl.sampler :refer [sampler!]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [hfdl.lang :refer [def fn $ binding vars main for local2 debug thread]])))

(defmacro vars "
Turns an arbitrary number of symbols resolving to vars into a map associating the fully qualified symbol
of this var to the value currently bound to this var.
" [& forms] (c/vars &env forms))

(def exports
  (vars hash-map vector list concat seq sort into first inc dec + - / * swap! cons sorted-map keys comp remove filter
    count map constantly
    m/eduction m/reductions m/relieve m/watch))

(def eval "Takes a resolve map and a program, returns a booting function.
The booting function takes
* as first argument a function Any->Task[Unit] returned task writes the value on the wire.
* as second argument a flow producing the values read on the wire.
and returning a task that runs the local reactor."
  r/eval)

(defmacro def [sym & body]
  (when-not (:js-globals &env)
    `(doto (~'def ~sym) (alter-meta! assoc :macro true :node (quote (do ~@body))))))

(defmacro main "
Takes a photon program and returns a pair
* the first item is the local booting function (cf eval)
* the second item is the remote program.
" [& body]
  (-> (c/analyze &env (cons 'do body))
    (update 0 (partial c/emit (comp symbol (partial str (gensym) '-))))
    (update 1 (partial list 'quote))))

(defmacro binding [bindings & body]
  (if-some [bindings (seq (partition-all 2 bindings))]
    (let [locals (repeatedly (count bindings) gensym)
          [vars exprs] (apply map vector bindings)]
      `(let [~@(interleave locals exprs)]
         (unquote ((def ~@vars) ~@(map (partial list 'var)
                                    (cons (cons 'do body) locals))))))
    (cons 'do body)))

;; TODO self-refer
(defmacro fn [args & body]
  (->> body
    (cons (vec (interleave args c/args)))
    (cons `let)
    (list 'var)))

; syntax quote doesn't qualify special forms like 'def
(defmacro defn [sym & rest] `(hfdl.lang/def ~sym (fn ~@rest)))

(defmacro $ [f & args]
  (->> args
    (map (partial list 'var))
    (cons f)
    (cons (cons 'def (take (count args) c/args)))
    (list `unquote)))

(defmacro for [bindings & body]
  (if-some [[s v & bindings] (seq bindings)]
    (->> (list 'var v)
      (list `rfor
        (list `comp
          (->> body
            (cons bindings)
            (cons `for)
            (list `let [s (second c/args)])
            (list 'var)
            (list `partial (list 'def (second c/args))))
          `r/steady))
      (list `unquote))
    (cons 'do body)))

; when lambdas work, thread will work for free because m/ap generates lambda
(defmacro thread [& body]
  `(unquote (m/ap (m/? (m/via m/blk ~@body)))))

(defmacro rec [& bindings])

(defmacro local2
  "2-peer loopback system with transfer. Returns boot task"
  [vars & body]
  `(let [[client# server#] (main ~@body)
         server# (r/eval ~vars server#)
         c->s# (m/rdv)
         s->c# (m/rdv)
         ServerReactor# (server# s->c# (m/sp (m/? (m/sleep 10 (m/? c->s#)))))
         ClientReactor# (client# c->s# (m/sp (m/? (m/sleep 10 (m/? s->c#)))))
         Reactors# (m/join {} ServerReactor# ClientReactor#)]
     Reactors#))

(defmacro local1
  "single peer system (no transfer, ~@ is undefined). Returns boot task"
  [& body]
  ; use compiler (client) because no need for exports
  `(let [[client# server#] (main ~@body)
         Reactor# (client# (constantly (m/sp)) m/never)]
     Reactor#))

(defmacro run "test entrypoint, single process" [& body]
  `(let [dispose# ((local1 ~@body)
                   (cc/fn [_#] #_(prn ::finished)) u/pst)]
     dispose#))

(defmacro run2 "test entrypoint, 2-peer loopback system"
  [vars & body]
  `(let [dispose# ((local2 ~vars ~@body)
                   (cc/fn [_#] #_(prn ::finished)) u/pst)]
     dispose#))

(cc/defn boot [f d]
  (cc/fn []
    (f nil)
    (sampler! f d)))

#?(:clj
   (defmacro debug [sym prg]
     `(boot
        (cc/fn [s#]
          (println (case s# nil :reset :ready))
          (def ~sym s#)) ~prg)))

#?(:clj
   (defmacro debug* [sym prg]
     `(boot (cc/fn [s#]
              (println (case s# nil :reset :ready))
              (reset! ~sym s#)) ~prg)))

(comment
  (def !input (atom "alice"))
  (cc/defn form-input [] (m/watch !input))
  (cc/defn render-table [>x] (m/relieve {}
                            (dataflow
                              (prn :render-table @>x))
                            #_(m/ap (prn :render-table (m/?! x)))))
  (cc/defn query [q] [q])

  ; what touches the network is eager
  ; if something is required remotely we don't lazy sample,
  ; we just pass the value eagerly and then when we receive it
  ; we can turn that into a lazy sampling.

  ; why? its debatable
  ; Leo says: think this is the behavior that will minimize
  ; experienced latency

  ; there are two types of effects with network between
  ; user interaction effects -> network -> rendering effects

  (def system-task
    (local2
      (vars !input form-input render-table query prn)
      (debug sampler
        (dataflow
          (let [needle @(form-input)
                results (remote (query needle))]
            @(render-table ~results))))))
  (system-task prn prn)
  ; no prints
  @sampler := nil
  ; :render-table ["alice"]
  (reset! !input "bob")
  ; nothing yet
  @sampler := nil
  ; :render-table ["bob"]
  )

