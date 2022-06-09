(ns hyperfiddle.photon
  (:refer-clojure :exclude [eval])
  (:require [hyperfiddle.photon-bootstrap :as p]
            [hyperfiddle.photon-impl.compiler :as c]
            [hyperfiddle.photon-impl.runtime :as r]
            [hyperfiddle.photon-impl.for :refer [map-by]]
            [missionary.core :as m]
            [clojure.core.async :as a]
            #?(:cljs [hyperfiddle.photon-client]))
  #?(:cljs (:require-macros [hyperfiddle.photon :refer [def defn fn vars main for for-by local local-with run run-with forget deduping]]))
  #?(:clj (:import (hyperfiddle.photon_impl.runtime Failure)
                   (hyperfiddle.photon Pending))))

(def client #?(:cljs hyperfiddle.photon-client/client))

#?(:clj
   (defn start-websocket-server! "returns server (jetty instance) for (.stop server)"
     ([] (start-websocket-server! nil))
     ([config] ((requiring-resolve 'hyperfiddle.photon-server/start!) config))))

(defmacro vars "
  Turns an arbitrary number of symbols resolving to vars into a map associating the fully qualified symbol
  of this var to the value currently bound to this var.
  " [& forms] (c/vars &env forms))

(defn merge-vars
  ([fa fb]
   (fn [not-found ident]
     (let [a (fa not-found ident)]
       (if (= not-found a)
         (fb not-found ident)
         a))))
  ([fa fb & fs]
   (reduce merge-vars (merge-vars fa fb) fs)))


(def eval "Takes a resolve map and a program, returns a booting function.
  The booting function takes
  * as first argument a function Any->Task[Unit] returned task writes the value on the wire.
  * as second argument a flow producing the values read on the wire.
  and returning a task that runs the local reactor."
  r/eval)

(def path r/path)

(defmacro main "
  Takes a photon program and returns a pair
  * the first item is the local booting function (cf eval)
  * the second item is the remote program.
  " [& body]
  (-> (c/analyze &env (cons 'do body))
      (update 0 (partial r/emit (comp symbol (partial str (gensym) '-))))
      (update 1 (partial list 'quote))))

(defmacro for-by [kf bindings & body]
  (if-some [[s v & bindings] (seq bindings)]
    (->> (list `p/fn [] v)
         (list `map-by kf
               (->> body
                    (list* `for-by kf bindings)
                    (list `let [s (second c/arg-sym)])
                    (list `p/fn [])
                    (list `partial (list 'def (second c/arg-sym)))))
         (list `new))
    (cons `do body)))

(defn pair [c s]
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
  `((local ~@body) (fn [_#]) (fn [_#])))

(defmacro run-with "test entrypoint with whitelist." [vars & body]
  `((local-with ~vars ~@body) (fn [_#]) (fn [_#])))

(p/defn Watch [!x]
  (new (m/watch !x)))

(defmacro watch "for tutorials (to delay teaching constructor syntax); m/watch is also idiomatic"
  [!x] `(new (m/watch ~!x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         EXPERIMENTAL ZONE             ;;
;;                                       ;;
;; Everything below should be considered ;;
;; guilty until proven innocent          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ^:no-doc continuous "EXPERIMENTAL"
  ([>x] (continuous nil >x))
  ([init >x] (m/relieve {} (m/reductions {} init >x))))

(defmacro ^:no-doc forget
  "EXPERIMENTAL
  Like `do` but returs `nil` once, then never return again."
  [& body]
  `(new (->> (hyperfiddle.photon/fn [] ~@body)
             (m/eduction (constantly nil) (dedupe))
             (m/reductions {} nil)
             (m/relieve {}))))

(defmacro ^:no-doc deduping "EXPERIMENTAL" [x]
  `(new (->> (hyperfiddle.photon/fn [] ~x)
             (m/eduction (dedupe))
             (m/reductions {} nil)
             (m/relieve {}))))

(defn ^:no-doc newest "EXPERIMENTAL" [>left >right] (m/ap (m/?< (m/amb= >left >right))))

(defn wrap "run slow blocking fn on a threadpool"
  [f & args]
  #?(:clj
     (->> (m/ap (m/? (m/via m/cpu (apply f args))))
          (m/reductions {} (Failure. (Pending.))))))

;; --------------------------------------
(defmacro def [& args] `(p/def ~@args))
(defmacro fn [& args] `(p/fn ~@args))
(defmacro defn [& args] `(p/defn ~@args))
(defmacro for [& args] `(p/for ~@args))
