(ns hfdl.lang
  (:refer-clojure :exclude [for eval])
  (:require [hfdl.impl.compiler :as c]
            [hfdl.impl.runtime :as r]
            [hfdl.impl.util :as u]
            [hfdl.impl.sampler :refer [sampler!]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [hfdl.lang :refer [defnode vars main for system debug node thread]])))

;; TODO variadic
(defmacro defnode "" [sym & decl]
  (let [args (gensym)]
    `(defmacro ~sym [& ~args]
       (c/node ~(name (ns-name *ns*))
         ~(name sym) (quote ~decl) ~args))))

(defmacro node [& x])

(defmacro thread [body]
  `(unquote (m/ap (m/? (m/via m/blk ~body)))))
; when lambdas work, thread will work for free because m/ap generates lambda

(defmacro main [& body]
  (c/main (gensym) &env (cons `do body)))

(defmacro for [bindings & body]
  (if-some [[s v & bindings] (seq bindings)]
    `(r/prod {s# ~v} (let [~s s#] (for ~bindings ~@body)))
    `(do ~@body)))

(defmacro bind [bindings & body])

(defnode call [g & args] #_(apply g args))

(defmacro vars "
Turns an arbitrary number of symbols resolving to vars into a map associating the fully qualified symbol
of this var to the value currently bound to this var.
" [& forms] (c/vars &env forms))

(def peer r/peer)
(def eval r/eval)

(def exports (vars hash-map vector list concat seq sort into first m/watch))

(defmacro system [vars & body]
  `(let [[c# s#] (main ~@body)
         s# (eval ~vars s#)]
     (m/sp
       (let [c->s# (m/rdv)
             s->c# (m/rdv)]
         (m/? (m/join {}
                (peer s# (-> s->c# #_(u/log-args 'r->l)) (u/poll c->s#))
                (peer c# (-> c->s# #_(u/log-args 'l->r)) (u/poll s->c#))))))))

(defmacro local-system
  "single peer system (no transfer) with sampler. ~@ is undefined"
  [& body]
  `(let [[c# _#] (main ~@body)]
     ; use compiler (client) because no need for exports
     (peer c# (constantly (m/sp)) m/none)))

(defmacro run "test entrypoint, single process" [& body]
  `(let [reactor-task# (local-system ~@body)
         dispose-reactor# (reactor-task#
                            (fn [_#] #_(prn ::finished)) u/pst)]
     dispose-reactor#))

(defn boot [f d]
  (fn []
    (f nil)
    (sampler! f d)))

#?(:clj
   (defmacro debug [sym prg]
     `(boot
        (fn [s#]
          (println (case s# nil :reset :ready))
          (def ~sym s#)) ~prg)))

#?(:clj
   (defmacro debug* [sym prg]
     `(boot (fn [s#]
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
    (system
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

