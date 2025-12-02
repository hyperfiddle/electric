(ns hyperfiddle.electric-local-def3
  (:refer-clojure :exclude [compile])
  #?(:cljs (:require-macros hyperfiddle.electric-local-def3))
  (:import #?(:clj [clojure.lang IFn IDeref ExceptionInfo])
           [missionary Cancelled])
  (:require [clojure.core :as cc]
            [contrib.assert :as ca]
            [contrib.cljs-target]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric.impl.lang3 :as lang]
            [hyperfiddle.electric.impl.runtime3 :as r]
            [hyperfiddle.incseq.flow-protocol-enforcer :as fpe]
            [contrib.debug :as dbg]
            #?(:clj [contrib.triple-store :as ts])
            #?(:clj [hyperfiddle.rcf.analyzer :as ana]) ; todo remove
            [missionary.core :as m]
            [hyperfiddle.detest :as dt]))

(defn ->local-config [env]
  (let [p (if (:js-globals env) :cljs :clj)] {::lang/peers {:client p, :server p}}))

#?(:clj (defmacro test-compile
          ([nm form] `(test-compile ~nm {} ~form))
          ([nm env form] `(lang/compile ~nm '~form (merge e/web-config (lang/normalize-env ~env))))))

#?(:clj (defn code->ts* [env conf form]
          (ca/check map? conf)
          (let [env (merge (->local-config env) (lang/normalize-env env) conf)
                expanded (lang/expand-all env `(::lang/ctor ~form))
                ts (lang/analyze expanded '_ env (lang/->ts))
                _  (when (::lang/print-analysis env) (run! prn (->> ts :eav vals (sort-by :db/id))))]
            (lang/analyze-electric env ts))))

#?(:clj (defmacro code->ts {:style/indent 1} [conf & body]
          `(code->ts* ~&env ~conf '(do ~@body))))

#?(:clj (defn ->env [env conf] (merge (->local-config env) (lang/normalize-env env) conf)))

(defmacro main {:style/indent 1} [conf & body]
  (ca/is conf map? "provide config map as first argument")
  `(r/->defs {::Main ~(lang/->source (->env &env conf) ::Main (lang/?meta (first body) `(e/fn [] (do ~@body))))}))

(defn fastest [& args]
  (m/absolve (apply m/race (map m/attempt args))))

(def run-local
  (letfn [(subject [^objects state slot]
            (fn [cb] (aset state slot cb) #()))
          (reader [^objects state slot]
            (m/ap (m/? (m/?> (m/seed (repeat (aget state slot)))))))
          (writer [^objects state slot latency events]
            (m/reduce (fn ([] (aget state slot))
                        ([cb x] (cb x) cb))
              (m/zip {} latency events)))]
    (fn [result-sample-clock
         client-reader-clock client-writer-clock
         server-reader-clock server-writer-clock
         defs main]
      (let [state (doto (object-array 4)
                    (aset 0 (m/mbx))                        ;; client->server
                    (aset 1 (m/mbx)))                       ;; server->client
            root (r/make-root :client defs main nil)]
        (fastest
          (m/reduce (constantly nil) (m/zip {} result-sample-clock (r/frame-result root)))
          ;; client process. Pushes its events into mbx at idx 0. Registers callback at idx 2
          (writer state 0 client-writer-clock (r/root-socket root {} (subject state 2)))
          ;; server process. Pushes its events into mbx at idx 1. Registers callback at idx 3
          (writer state 1 server-writer-clock
            (r/root-socket (r/make-root :server defs main nil) {} (subject state 3)))
          ;; polls the client->server mailbox and pushes values in the server callback
          (writer state 3 server-reader-clock (reader state 0))
          ;; polls the server->client mailbox and pushes values in the client callback
          (writer state 2 client-reader-clock (reader state 1)))))))

(def immediate (m/seed (repeat nil)))

(defmacro local {:style/indent 1} [conf & body]
  `(run-local
     ~(::lang/result-sample-clock conf `immediate)
     ~(::lang/client-reader-clock conf `immediate)
     ~(::lang/client-writer-clock conf `immediate)
     ~(::lang/server-reader-clock conf `immediate)
     ~(::lang/server-writer-clock conf `immediate)
     (main ~conf ~@body) ::Main))

(defprotocol Engine
  (spawn [this flow])
  (step [this pred])
  (tap [this v])
  (cancel [this])
  (->rng [this])
  (->info [this])
  (->opts [this])
  (->dbgf [this])
  (add-proc [this proc])
  (del-proc [this proc]))

(defn instrument [nm ngn flow]
  (fn [step done]
    (let [it ((dbg/instrument* nm (->dbgf ngn) flow) step done)]
      (reify
        IFn
        (#?(:clj invoke :cljs -invoke) [_] (it))
        (#?(:clj invoke :cljs -invoke) [_ n] ((it :process) n))
        IDeref
        (#?(:clj deref :cljs -deref) [_] @it)))))

(defn ->queue
  ([] #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))
  ([& args] (into (->queue) args)))

(defn ->engine
  ([] (->engine {}))
  ([{:keys [seed] :as o}]
   (let [seed (or seed (dt/random-seed)), rng (dt/->xorshift64 seed), !proc* (atom [])
         !n (atom 0), dbgf (case (:debug o)
                             (:steps) (fn [_] (swap! !n inc))
                             (:full) (fn [x] (swap! !n inc) (prn x))
                             #_else prn)
         !s (atom nil), !root (atom nil)
         !q (atom (->queue))]
     (reify Engine
       (add-proc [_ proc] (swap! !proc* conj proc))
       (del-proc [_ proc] (swap! !proc* (fn [proc*] (filterv #(not= % proc) proc*))))
       (tap [_ v] (swap! !q conj v))
       (->rng [_] rng)
       (->opts [_] o)
       (->info [_] {:seed seed, :steps @!n})
       (->dbgf [_] dbgf)
       (spawn [this flow]
         (let [flow (fpe/enforce {:name 'root, :on-violate dt/on-violate}
                      (cond->> flow (-> this ->opts :debug) (dbg/instrument* 'root dbgf)))]
           (reset! !root ((m/reduce (fn [_ _] (reset! !s :step)) nil flow) {} {}))
           #_(reset! !root ((->> flow
                            ;; workaround for https://www.notion.so/hyperfiddle/delayed-local-sampling-of-a-conditional-can-glitch-on-remote-peer-17db4d1e85d18038bfcff5d5c0bbe847?pvs=4
                            (m/latest identity)          ; collapse consecutive transfers
                            (m/eduction (map identity))) ; consume eagerly
                          #(reset! !s :step) #(reset! !s :done)))
           nil))
       (cancel [this] (@!root) (tap this ::cancelled))
       (step [_ pred]
         ;; (prn 'q-before-step (into [] @!q))
         (try
           (let [v (loop [idle* #{}, proc* @!proc*]
                     (let [q @!q]
                       (if (seq q)
                         (let [v (peek q)]
                           (if (pred v)
                             (do (swap! !q pop) v)
                             (throw (ex-info (str "value " (pr-str v) " doesn't match predicate " pred) {:seed seed, :steps @!n, :value v, :predicate pred}))))
                         (if (= :step @!s)
                           (do (reset! !s nil) #_@@!root (recur #{} @!proc*))
                           (if (and (not= :done @!s) (seq proc*))
                             (let [n (rng (count proc*)), proc (nth proc* n)]
                               (if (proc (rng))
                                 (recur #{} @!proc*)
                                 (recur (conj idle* proc) (into [] (remove #{proc}) proc*))))
                             (throw (ex-info "predicate not reached" {:pred pred, :queue (into [] @!q)})))))))]
             ;; randomly forward some clocks
             (doseq [proc @!proc*] (when (zero? (rng 4)) (proc (rng))))
             v)
           (catch Cancelled e (throw e))
           (catch #?(:clj Throwable :cljs :default) e (throw (ex-info "step failed" {:seed seed, :steps @!n} e)))))))))

(defn clock [ngn nm]
  (cond->> (fn [step done]
             (step)                     ; TODO should the clock be initialized?
             (let [!should-step? (atom false), !done? (atom false), !cancelled? (atom false)
                   fin #(when-not (first (reset-vals! !done? true)) (done))
                   proc
                   (reify
                     IFn
                     (#?(:clj invoke :cljs -invoke) [_]
                       (let [cancelled? (first (reset-vals! !cancelled? true))]
                         (when (and @!should-step? (not @!done?) (not cancelled?))
                           (swap! !should-step? not) (step))))
                     (#?(:clj invoke :cljs -invoke) [this _]
                       (if @!done?
                         (del-proc ngn this)
                         (when (and @!should-step? (not @!done?)) (swap! !should-step? not) (step) :stepped)))
                     IDeref
                     (#?(:clj deref :cljs -deref) [_]
                       (cond
                         @!done?
                         (throw (ex-info "transfer after done" {}))

                         @!cancelled?
                         (do (fin) (throw (Cancelled.)))

                         @!should-step?
                         (throw (ex-info "transfer without step" {}))

                         :else (do (swap! !should-step? not) 'tick))))]
               (add-proc ngn proc)
               proc))
    (-> ngn ->opts :debug) (instrument nm ngn)))

(letfn [(subject [^objects state slot]
          (fn [cb] (aset state slot cb) #(aset state slot nil)))
        (reader [^objects state slot]
          (m/ap (m/? (m/?> (m/seed (repeat (aget state slot)))))))
        (clocked [ngn nm flow] (m/zip {} (clock ngn nm) flow))
        (drain [flow] (m/ap (m/amb nil (do (m/?> flow) (m/amb)))))
        (writer [^objects state slot events]
          (m/ap (let [cb (ca/is (aget state slot))] (cb (m/?> events)))))]
  (defn local-flow [ngn defs main]
    (m/ap
      (let [state (doto (object-array 4) (aset 0 (m/mbx)) (aset 1 (m/mbx)))
            c (r/make-root :client defs main nil)
            s (r/make-root :server defs main nil)
            _ (m/?> (drain (writer state 0 (clocked ngn 'client-writer (r/root-socket c {} (subject state 2))))))
            _ (m/?> (drain (writer state 1 (clocked ngn 'server-writer (r/root-socket s {} (subject state 3))))))
            _ (m/?> (drain (writer state 2 (clocked ngn 'client-reader (reader state 1)))))
            _ (m/?> (drain (writer state 3 (clocked ngn 'server-reader (reader state 0)))))]
        (m/?> (r/frame-result c))))))

(defmacro local-ngn {:style/indent 2} [conf ngn & body]
  `(local-flow ~ngn (main ~conf ~@body) ::Main))

(defn run-single [defs main]
  (r/sink (r/frame-result (r/make-root :client defs main nil))))

(defmacro single {:style/indent 1} [conf & body]
  (with-meta `(run-single (main ~conf ~@body) ::Main) (meta &form)))
