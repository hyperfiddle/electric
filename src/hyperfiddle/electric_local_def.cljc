(ns hyperfiddle.electric-local-def
  (:refer-clojure :exclude [def defn])
  #?(:cljs (:require-macros hyperfiddle.electric-local-def))
  (:require
   [clojure.core :as cc]
   [clojure.pprint :as pp]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric.impl.ir-utils :as ir-utils]
   [hyperfiddle.electric.impl.lang :as lang]
   [hyperfiddle.electric.impl.runtime :as r]
   #?(:clj [hyperfiddle.rcf.analyzer :as ana]) ; todo remove
   [missionary.core :as m]))

#?(:clj
   (do
     ;; Optionally, tell RCF not to rewrite Electric programs.
     (defmethod ana/macroexpand-hook `hyperfiddle.electric-local-def/local [the-var form env args] (reduced `(hyperfiddle.electric-local-def/local ~@args)))))

(cc/defn ->local-config [env]
  (let [p (if (:js-globals env) :cljs :clj)] {::lang/peers {:client p, :server p}, ::lang/current :client}))

(cc/defn pair [c s]
  (m/sp
    (let [s->c (m/dfv)
          c->s (m/dfv)]
      (m/?
        (m/join {}
          (s (cc/fn [x] (m/sp ((m/? s->c) x)))
            (cc/fn [!] (c->s !) #()))
          (c (cc/fn [x] (m/sp ((m/? c->s) x)))
            (cc/fn [!] (s->c !) #())
            #(throw %)))))))

(cc/defn pairt [c s]
  (m/sp
    (let [s->c (m/dfv)
          c->s (m/dfv)]
      (m/?
        (m/join {}
          (s (cc/fn write [x]
               (println "----")
               (prn :server-write (:acks x))
               (run! prn (:tree x))
               (let [ch (:change x)] (when (seq ch) (prn :change ch)))
               (let [fr (:freeze x)] (when (seq fr) (prn :freeze fr)))
               (m/sp ((m/? s->c) x)))
            (cc/fn ?read [!] (c->s !) #()))
          (c (cc/fn write [x]
               (println "----")
               (prn :client-write (:acks x))
               (run! prn (:tree x))
               (let [ch (:change x)] (when (seq ch) (prn :change ch)))
               (let [fr (:freeze x)] (when (seq fr) (prn :freeze fr)))
               (m/sp ((m/? c->s) x)))
            (cc/fn ?read [!] (s->c !) #())
            #(throw %)))))))

#?(:clj
   (defmacro local
     "Single peer loopback system without whitelist. Returns boot task."
     {:style/indent 0}
     [& body]
     (let [env (e/normalize-env &env)
           client (lang/analyze (merge env (->local-config env) {::lang/me :client}) `(do ~@body))
           client-info (r/compile "clocal" client)
           server (lang/analyze (merge env (->local-config env) {::lang/me :server}) `(do ~@body))
           server-info (r/compile "slocal" server)]
       `(pair
          (r/main ~client-info)
          (r/main ~server-info)))))
#?(:clj
   (defmacro run "test entrypoint without whitelist."
     {:style/indent 0}
     [& body]
     `((local ~@body) (cc/fn [_#]) (cc/fn [_#]))))
#?(:clj
   (defmacro locald
     "Single peer loopback system without whitelist. Returns boot task."
     {:style/indent 0}
     [& body]
     (let [env (e/normalize-env &env)
           client (lang/analyze (merge env (->local-config env) {::lang/me :client}) `(do ~@body))
           _ (do (println "--- CLIENT IR ---") (pp/pprint (ir-utils/unwrite client)))
           client-info (r/compile "clocal" client)
           _ (do (println "--- CLIENT SOURCE ---") (pp/pprint (pp/pprint client-info)))
           server (lang/analyze (merge env (->local-config env) {::lang/me :server}) `(do ~@body))
           _ (do (println "--- SERVER IR ---") (pp/pprint (ir-utils/unwrite server)))
           server-info (r/compile "slocal" server)
           _ (do (println "--- SERVER SOURCE ---") (pp/pprint (pp/pprint server-info)))]
       `(pair
          (r/main ~client-info)
          (r/main ~server-info)))))
#?(:clj
   (defmacro localt
     "Single peer loopback system without whitelist. Returns boot task."
     {:style/indent 0}
     [& body]
     (let [env (e/normalize-env &env)
           client (lang/analyze (merge env (->local-config env) {::lang/me :client}) `(do ~@body))
           client-info (r/compile "clocal" client)
           server (lang/analyze (merge env (->local-config env) {::lang/me :server}) `(do ~@body))
           server-info (r/compile "slocal" server)]
       `(pairt
          (r/main ~client-info)
          (r/main ~server-info)))))

(defmacro def
  ([symbol] `(hyperfiddle.electric-local-def/def ~symbol [::lang/unbound '~(cc/symbol (str *ns*) (str symbol))]))
  ([symbol docstring init]
   (assert (string? docstring))
   (#'def &form &env (vary-meta symbol assoc :doc docstring) init))
  ([symbol init] (lang/-def (merge (e/normalize-env &env) (->local-config &env)) symbol init)))

(defmacro defn [sym & fdecl]
  (let [[_defn sym' & _] (macroexpand `(cc/defn ~sym ~@fdecl))] ; GG: docstring support
    `(hyperfiddle.electric-local-def/def ~sym' (e/fn ~(vary-meta sym' merge (meta &form) (meta sym'))
                                                 ~@(if (string? (first fdecl)) ; GG: skip docstring
                                                     (rest fdecl)
                                                     fdecl)))))
