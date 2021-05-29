(ns hfdl.lang
  (:require #?(:clj [hfdl.impl.compiler :as c])
            [hfdl.impl.runtime :as r]
            [hfdl.impl.util :as u]
            [hfdl.impl.sampler :refer [sampler!]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [hfdl.lang :refer [dataflow vars]])))

(defmacro dataflow
  "Defines a dataflow program from given HFDL expressions, in an implicit `do`."
  [& body]
  (c/df (gensym) &env (cons `do body)))

(defmacro vars "
Turns an arbitrary number of symbols resolving to vars into a map associating the fully qualified symbol
of this var to the value currently bound to this var.
" [& forms] (c/vars &env forms))

(def client r/client)
(def server r/server)

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
  ; Boot a peer (process reactor) with a boot function
  ; the boot function is for a dag
  ; the debug adds a way to sample the result on the side for testing
  ; (in production the result is never sampled, process will fire more effects)

  ((peer
     (globals *)
     (debug sampler (dataflow (* 6 7)))                 ; dag boot fn
     #(m/sp (prn %))                                        ; trace spout
     (u/poll m/never))                                      ; no network inputs
   prn prn)

  @sampler

  )

(defn system [resolve boot]
  ; test emulator of distributed system
  (m/sp
    (let [l->r (m/rdv)
          r->l (m/rdv)]
      (m/? (m/join vector
             (server resolve (-> r->l #_(u/log-args 'r->l)) (u/poll l->r))
             (client boot (-> l->r #_(u/log-args 'l->r)) (u/poll r->l)))))))

(tests

  (def !a1 (atom 6))
  (def !a2 (atom 7))

  (def env (vars * m/watch !a1 !a2))
  (def dag (dataflow (* @(m/watch !a1) @(m/watch !a2))))

  ; repl server – run for effects but with hook for user to read the result at some time
  (def system-task (system env (fn [] (sampler! #(def sampler %) dag))))
  (system-task prn prn)                                     ; runs forever
  @sampler := 42
  @sampler := 42

  (swap! !a1 inc)
  @sampler := 49

  )

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

