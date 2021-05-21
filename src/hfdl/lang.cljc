(ns hfdl.lang
  (:require #?(:clj [hfdl.impl.compiler :as c])
            [hfdl.impl.trace :as t]
            [hfdl.impl.util :as u]
            [hfdl.impl.sampler :refer [sampler!]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:require-macros [hfdl.lang :refer [vars]])))

#?(:clj
   (defmacro remote [& body]
     `(unquote-splicing (do ~@body))))

#?(:clj
   (defmacro dataflow
     "Defines a dataflow program from given HFDL expressions, in an implicit `do`."
     [& body]
     (c/df &env (cons `do body))))

#?(:clj
   (defmacro vars "
Turns an arbitrary number of symbols resolving to vars into a map associating the fully qualified symbol
of this var to the value currently bound to this var.
" [& forms] (c/vars &env forms)))

(def exports (vars apply vector hash-map first next seq get keyword nil? m/relieve))

(def peer t/peer)

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

#?(:clj
   (defmacro df [& body]
     `(:expression (dataflow ~@body))))

(defn shell [s f]
  (let [in (m/mbx)
        out (m/rdv)
        cancel ((peer u/nop out (u/poll in)) s f)]
    (fn
      ([] (cancel))
      ([e] (in e))
      ([s f] (out s f)))))

(comment

  (def $ (shell prn prn))
  ($ (sample #{(df (* 6 7))}))
  (m/? $) := (change {(df (* 6 7)) 42})

  ($ (sample #{(df (* 6 (remote 7)))}))
  (m/? $) := (sample #{(df 7)})

  ($ (change {(df 7) 7}))
  (m/? $) := (change {(df (* 6 (remote 7))) 42})

  ($ (change {(df 7) 8}))
  (m/? $) := (change {(df (* 6 (remote 7))) 48})
  )



(defn system [resolve boot]
  ; test emulator of distributed system
  (m/sp
    (let [l->r (m/rdv)
          r->l (m/rdv)]
      (m/? (m/join vector
             (peer resolve #() (-> r->l #_(u/log-args 'r->l)) (u/poll l->r))
             (peer resolve boot (-> l->r #_(u/log-args 'l->r)) (u/poll r->l)))))))

(tests

  (def !a1 (atom 6))
  (def !a2 (atom 7))

  (def env (vars * m/watch !a1 !a2))
  (def dag (dataflow (* @(m/watch !a1) @(m/watch !a2))))

  ; does nothing, the boot had no effect and the return value is ignored
  ;(def system-task (system (fn [] dag)))

  ; prod server - run for effects, don't ever sample, that's fine
  (def system-task (system env (fn [] (dag #() #()))))

  ; repl server – run for effects but with hook for user to read the result at some time
  (def system-task (system env (fn [] (sampler! #(def sampler %) dag))))
  (system-task prn prn)                                     ; runs forever
  @sampler := 42
  @sampler := 42

  (swap! !a1 inc)
  @sampler := 49

  )

(tests
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



#_
    (defn dbg! ;; Follow the pr/print and  prn/println clojure.core idiom.
      "Runs given dataflow program in debug mode and returns a process instance.
      `dbg!` is for machines, use `debug!` for a human-friendly representation."
      ([program]      (dbg! program {}))
      ([program opts] (d/debug! program opts)))

#_
    (defn debug!
      "Runs given dataflow program in debug mode and returns a human-readable process
      instance."
      [program]
      (dbg! program {:source-mapped true}))

#_
    (defn heap-dump [process]
      (reduce merge (map meta (:log process))))

#_
    (defn result
      ([process-snapshot]
       (result (:program (meta process-snapshot)) process-snapshot))
      ([program process-snapshot]
       (get (heap-dump process-snapshot) [(:result program)])))

