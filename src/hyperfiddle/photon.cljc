(ns hyperfiddle.photon
  (:refer-clojure :exclude [def fn for eval defn])
  (:require [clojure.core :as cc]
            [hyperfiddle.photon-impl.compiler :as c]
            [hyperfiddle.photon-impl.runtime :as r]
            [hyperfiddle.photon-impl.sampler :refer [sampler!]]
            [hyperfiddle.photon-impl.for :refer [map-by]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]
            #?(:cljs [hyperfiddle.photon-client]))
  #?(:cljs (:require-macros [hyperfiddle.photon :refer [def defn fn vars main for for-by local local-with run run-with forget deduping]])))

(def client #?(:cljs hyperfiddle.photon-client/client))

#?(:clj
   (cc/defn start-server!
     ([] (start-server! nil))
     ([config] ((requiring-resolve 'hyperfiddle.photon-server/start!) config))))

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
(defmacro defn [sym & fdecl]
  (let [[_defn sym & _] (macroexpand `(cc/defn ~sym ~@fdecl))] ; GG: Support IDE documentation on hover
    `(hyperfiddle.photon/def ~sym (fn ~@(if (string? (first fdecl)) ; GG: skip docstring
                                          (rest fdecl)
                                          fdecl)))))

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

(hyperfiddle.photon/defn Watch [!x]
  (new (m/watch !x)))

(defmacro watch "for tutorials (to delay teaching constructor syntax); m/watch is also idiomatic"
  [!x] `(new (m/watch ~!x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         EXPERIMENTAL ZONE             ;;
;;                                       ;;
;; Everything below should be considered ;;
;; guilty until proven innocent          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; G: Experiments were moved by Léo from hyperfiddle.photon to a dedicated
;;    hyperfiddle.photon-xp namespace. Dustin and Geoffrey decided to move them
;;    back to hyperfiddle.photon and tag them as experimental to reduce user's
;;    cognitive load when exploring the codebase.

(cc/defn ^:no-doc continuous "EXPERIMENTAL"
  ([>x] (continuous nil >x))
  ([init >x] (m/relieve {} (m/reductions {} init >x))))

(defmacro ^:no-doc forget
  "EXPERIMENTAL
  Like `do` but returs `nil` once, then never return again."
  [& body]
  `(new (->> (p/fn [] ~@body)
             (m/eduction (constantly nil) (dedupe))
             (m/reductions {} nil)
             (m/relieve {}))))

(defmacro ^:no-doc deduping "EXPERIMENTAL" [x]
  `(new (->> (p/fn [] ~x)
             (m/eduction (dedupe))
             (m/reductions {} nil)
             (m/relieve {}))))

(cc/defn ^:no-doc newest "EXPERIMENTAL" [>left >right] (m/ap (m/?< (m/amb= >left >right))))
