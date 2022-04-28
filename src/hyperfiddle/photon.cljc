(ns hyperfiddle.photon
  (:refer-clojure :exclude [def fn for eval defn])
  (:require [clojure.core :as cc]
            [hyperfiddle.photon-impl.compiler :as c]
            [hyperfiddle.photon-impl.runtime :as r]
            [hyperfiddle.photon-impl.sampler :refer [sampler!]]
            [hyperfiddle.photon-impl.for :refer [map-by]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [hyperfiddle.photon :refer [def defn fn vars main for for-by local local-with run run-with]])))

(defmacro vars "
Turns an arbitrary number of symbols resolving to vars into a map associating the fully qualified symbol
of this var to the value currently bound to this var.
" [& forms] (c/vars &env forms))

(cc/defn merge-vars
  ([fa fb]
   (cc/fn [not-found ident]
     (let [a (fa not-found ident)]
       (if (= not-found a)
         (fb not-found ident)
         a))))
  ([fa fb & fs]
   (reduce merge-vars (merge-vars fa fb) fs)))

(def exports
  (vars hash-map vector list concat seq sort into first next inc dec + - / * < swap! cons identity nth
    sorted-map keys comp remove filter map constantly str coll? empty list? map? partial count ex-info some-fn
    m/eduction m/reductions m/relieve m/watch map-by r/failure r/steady r/recover r/clause r/fail r/latest-first))

(def eval "Takes a resolve map and a program, returns a booting function.
The booting function takes
* as first argument a function Any->Task[Unit] returned task writes the value on the wire.
* as second argument a flow producing the values read on the wire.
and returning a task that runs the local reactor."
  r/eval)

(defmacro def
  ([sym] `(hyperfiddle.photon/def ~sym ::c/unbound))
  ([sym form] 
   ;; GG: Expand to an unbound var with body stored in ::c/node meta.
   ;;     Clojure compiler will analyze vars metas, which would analyze form as clojure, so we quote it.
   ;;     ClojureScript do not have vars at runtime and will not analyze or emit vars meta. No need to quote.
   `(def ~(vary-meta sym assoc ::c/node (if (:js-globals &env) form `(quote ~form))))))

(def path r/path)

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
    (cons (vec (interleave args (next c/arg-sym))))
    (cons `let)
    (list ::c/closure)))

; syntax quote doesn't qualify special forms like 'def
(defmacro defn [sym & rest] `(hyperfiddle.photon/def ~sym (fn ~@rest)))

(defmacro for-by [kf bindings & body]
  (if-some [[s v & bindings] (seq bindings)]
    (->> (list `fn [] v)
      (list `map-by kf
        (->> body
          (list* `for-by kf bindings)
          (list `let [s (second c/arg-sym)])
          (list `fn [])
          (list `partial (list 'def (second c/arg-sym)))))
      (list `new))
    (cons `do body)))

(defmacro for [bindings & body]
  `(for-by identity ~bindings ~@body))

(cc/defn pair [c s]
  (let [c->s (m/rdv)
        s->c (m/rdv)]
    (m/join {}
      (s s->c (m/sp (m/? (m/sleep 10 (m/? c->s)))))
      (c c->s (m/sp (m/? (m/sleep 10 (m/? s->c))))))))

(defmacro local
  "Single peer loopback system without whitelist. Returns boot task."
  [& body]
  ; use compiler (client) because no need for exports
  `(let [[client# server#] (main ~@body)]
     (pair client# (r/eval server#))))

(defmacro local-with
  "Single peer loopback system with whitelist. Returns boot task."
  [vars & body]
  `(let [[client# server#] (main ~@body)]
     (pair client# (r/eval ~vars server#))))

(defmacro run "test entrypoint without whitelist." [& body]
  `((local ~@body) (cc/fn [_#]) (cc/fn [_#])))

(defmacro run-with "test entrypoint with whitelist." [vars & body]
  `((local-with ~vars ~@body) (cc/fn [_#]) (cc/fn [_#])))