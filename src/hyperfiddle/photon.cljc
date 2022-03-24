(ns hyperfiddle.photon
  (:refer-clojure :exclude [def fn for eval defn])
  (:require [clojure.core :as cc]
            [hyperfiddle.photon-impl.compiler :as c]
            [hyperfiddle.photon-impl.runtime :as r]
            [hyperfiddle.photon-impl.sampler :refer [sampler!]]
            [hyperfiddle.photon-impl.for :refer [map-by]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [hyperfiddle.photon :refer [def defn fn $ vars main for for-by local1 local2 run run2]])))

(defmacro vars "
Turns an arbitrary number of symbols resolving to vars into a map associating the fully qualified symbol
of this var to the value currently bound to this var.
" [& forms] (c/vars &env forms))

(cc/defn merge-vars
  ([fa fb]
   (cc/fn
     ([ident] (or (fa ident) (fb ident)))
     ([not-found ident]
      (let [a (fa not-found ident)]
        (if (= not-found a)
          (fb not-found ident)
          a)))))
  ([fa fb & fs]
   (reduce merge-vars (merge-vars fa fb) fs)))

(def exports
  (vars hash-map vector list concat seq sort into first inc dec + - / * swap! cons identity nth
    sorted-map keys comp remove filter map constantly str coll? empty list? map? partial count
    m/eduction m/reductions m/relieve m/watch map-by r/failure r/steady))

(def eval "Takes a resolve map and a program, returns a booting function.
The booting function takes
* as first argument a function Any->Task[Unit] returned task writes the value on the wire.
* as second argument a flow producing the values read on the wire.
and returning a task that runs the local reactor."
  r/eval)

(defmacro def [sym & body]
  (when-not (:js-globals &env)
    `(~'def ~(vary-meta sym assoc :macro true ::c/node `(quote (do ~@body))))))

(defmacro main "
Takes a photon program and returns a pair
* the first item is the local booting function (cf eval)
* the second item is the remote program.
" [& body]
  (-> (c/analyze &env (cons 'do body))
    (update 0 (partial r/emit (comp symbol (partial str (gensym) '-))))
    (update 1 (partial list 'quote))))

;; TODO self-refer
(defmacro fn [args & body]
  (->> body
    (cons (vec (interleave args c/args)))
    (cons `let)
    (list 'var)))

; syntax quote doesn't qualify special forms like 'def
(defmacro defn [sym & rest] `(hyperfiddle.photon/def ~sym (fn ~@rest)))

(defmacro $ [f & args]
  `(binding [~@(interleave c/args args)] (unquote ~f)))

(defmacro for-by [kf bindings & body]
  (if-some [[s v & bindings] (seq bindings)]
    (->> (list 'var v)
      (list `map-by kf
        (->> body
          (list* `for-by kf bindings)
          (list `let [s (second c/args)])
          (list 'var)
          (list `partial (list 'def (second c/args)))))
      (list `unquote))
    (cons 'do body)))

(defmacro for [bindings & body]
  `(for-by identity ~bindings ~@body))

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
  `(let [dispose# ((local1 ~@body) (cc/fn [_#]) (cc/fn [_#]))]
     dispose#))

(defmacro run2 "test entrypoint, 2-peer loopback system"
  [vars & body]
  `(let [dispose# ((local2 ~vars ~@body) (cc/fn [_#]) (cc/fn [_#]))]
     dispose#))