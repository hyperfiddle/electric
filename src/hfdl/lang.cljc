(ns hfdl.lang
  (:refer-clojure :exclude [fn for eval])
  (:require [clojure.core :as cc]
            [hfdl.impl.compiler :as c]
            [hfdl.impl.runtime :as r]
            [hfdl.impl.util :as u]
            [hfdl.impl.rfor :refer [rfor]]
            [hfdl.impl.sampler :refer [sampler!]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [hfdl.lang :refer [defnode bind vars main for local2 debug node thread]])))

(alter-meta!
  (intern *ns* 'def
    (cc/fn [_ _ sym & body]
      `(doto (def ~sym) (alter-meta! assoc :macro true :node (quote ~body)))))
  assoc :macro true)

(defmacro main [& body]
  (-> (c/analyze &env (cons `do body))
    (update 0 (partial c/emit (comp symbol (partial str (gensym) '-))))
    (update 1 (partial list `quote))))

;; TODO self-refer
(defmacro fn [args & body]
  `(partial (def ~@(take (count args) c/args))
     (var (let [~@(interleave args c/args)] ~@body))))

(defmacro $ [f & args]
  `(unquote (~f ~@(map (partial list `var) args))))

(defmacro for [bindings & body]
  (if-some [[s v & bindings] (seq bindings)]
    `(unquote (rfor (comp (partial (def ~(second c/args))
                            (var (let [~s ~(second c/args)]
                                   (for ~bindings ~@body))))
                      r/steady) (var ~v)))
    `(do ~@body)))

; when lambdas work, thread will work for free because m/ap generates lambda
(defmacro thread [& body]
  `(unquote (m/ap (m/? (m/via m/blk ~@body)))))

(def eval r/eval)

(defmacro vars "
Turns an arbitrary number of symbols resolving to vars into a map associating the fully qualified symbol
of this var to the value currently bound to this var.
" [& forms] (c/vars &env forms))

(def exports (vars hash-map vector list concat seq sort into first inc dec + - / * m/watch))

(defmacro local2
  "2-peer loopback system with transfer. Returns boot task"
  [vars & body]
  `(let [[client# server#] (main ~@body)
         server# (eval ~vars server#)
         c->s# (m/rdv)
         s->c# (m/rdv)
         ServerReactor# (server# (-> s->c# #_(u/log-args 'r->l)) (u/poll c->s#))
         ClientReactor# (client# (-> c->s# #_(u/log-args 'l->r)) (u/poll s->c#))
         Reactors# (m/join {} ServerReactor# ClientReactor#)]
     Reactors#))

(defmacro local1
  "single peer system (no transfer, ~@ is undefined). Returns boot task"
  [& body]
  ; use compiler (client) because no need for exports
  `(let [[client# server#] (main ~@body)
         Reactor# (client# (constantly (m/sp)) m/none)]
     Reactor#))

(defmacro run "test entrypoint, single process" [& body]
  `(let [dispose# ((local1 ~@body)
                   (cc/fn [_#] #_(prn ::finished)) u/pst)]
     dispose#))

(defmacro run2 "test entrypoint, 2-peer loopback system"
  [vars & body]
  `(let [dispose ((local2 vars ~@body)
                  (cc/fn [_#] #_(prn ::finished)) u/pst)]
     dispose))

(defn boot [f d]
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
  (defn form-input [] (m/watch !input))
  (defn render-table [>x] (m/relieve {}
                            (dataflow
                              (prn :render-table @>x))
                            #_(m/ap (prn :render-table (m/?! x)))))
  (defn query [q] [q])

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

