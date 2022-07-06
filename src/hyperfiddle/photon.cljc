(ns hyperfiddle.photon
  (:refer-clojure :exclude [eval def defn fn for])
  (:require [clojure.core :as cc]
            [hyperfiddle.photon-impl.compiler :as c]
            [hyperfiddle.photon-impl.runtime :as r]
            [hyperfiddle.photon-impl.for :refer [map-by]]
            #?(:clj [hyperfiddle.rcf.analyzer :as ana])     ; todo remove
            [missionary.core :as m]
            [clojure.core.async :as a]
            #?(:cljs [hyperfiddle.photon-client]))
  #?(:cljs (:require-macros [hyperfiddle.photon :refer [def defn fn vars main for for-by local local-with run run-with forget deduping debounce wrap]]))
  (:import #?(:clj (clojure.lang IDeref))
           (hyperfiddle.photon Pending Failure)
           (missionary Cancelled)))

#?(:clj
   (do
                                        ; Optionally, tell RCF not to rewrite Photon programs.
     (defmethod ana/macroexpand-hook `hyperfiddle.photon/run [the-var form env args] `(hyperfiddle.photon/run ~@args)) ; optional
                                        ;(defmethod ana/macroexpand-hook `hyperfiddle.photon/run2 [_the-var _form _env args] `(hyperfiddle.photon/run2 ~@args))

                                        ; Don't expand cc/binding (prevent infinite loop). Explicit implicit do
     (defmethod ana/macroexpand-hook 'clojure.core/binding [_the-var _form _env [bindings & body]] (reduced `(binding ~bindings (do ~@body))))
     (defmethod ana/macroexpand-hook 'cljs.core/binding [_the-var _form _env [bindings & body]] (reduced `(binding ~bindings (do ~@body))))))



(def client #?(:cljs hyperfiddle.photon-client/client))

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

(def hook r/hook)
(def bind r/bind)
(def with r/with)

(defmacro def
  ([sym] `(hyperfiddle.photon/def ~sym ::c/unbound))
  ([sym form] 
   ;; GG: Expand to an unbound var with body stored in ::c/node meta.
   ;;     Clojure compiler will analyze vars metas, which would analyze form as clojure, so we quote it.
   ;;     ClojureScript do not have vars at runtime and will not analyze or emit vars meta. No need to quote.
   `(def ~(vary-meta sym assoc ::c/node (if (:js-globals &env) form `(quote ~form))))))

(defmacro main "
  Takes a photon program and returns a pair
  * the first item is the local booting function (cf eval)
  * the second item is the remote program.
  " [& body]
  (-> (c/analyze &env (cons 'do body))
    (update 0 (partial r/emit (gensym)))
    (update 1 (partial list 'quote))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         EXPERIMENTAL ZONE             ;;
;;                                       ;;
;; Everything below should be considered ;;
;; guilty until proven innocent          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cc/defn ^:no-doc continuous "EXPERIMENTAL"
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

(cc/defn ^:no-doc newest "EXPERIMENTAL" [>left >right] (m/ap (m/?< (m/amb= >left >right))))

(cc/defn wrap* [f & args]
  #?(:clj
     (->> (m/ap (m/? (m/via m/blk (apply f args))))
          (m/reductions {} (Failure. (Pending.)))
          (m/relieve {}))))

(defmacro wrap "Run blocking function (io-bound) on a threadpool"
  [f & args]
  `(new (wrap* ~f ~@args)))

;; Core.async interop.
;; Photon doesn't depend on core.async, this interop should move to a separate namespace or repo.
;; Keeping it here for simple, straight-to-the-point user demos.

(cc/defn chan-read
  "Return a task taking a value from `chan`. Retrun nil if chan is closed. Does
   not close chan, and stop reading from it when cancelled."
  [chan]
  (cc/fn [success failure] ; a task is a 2-args function, success and failure are callbacks.
    (let [cancel-chan (a/chan)] ; we will put a value on this chan to cancel reading from `chan`
      (a/go (let [[v port] (a/alts! [chan cancel-chan])] ; race between two chans
              (if (= port cancel-chan) ; if the winning chan is the cancelation one, then task has been cancelled
                (failure (Cancelled.)) ; task has been cancelled, must produce a failure state
                (success v) ; complete task with value from chan
                )))
      ;; if this task is cancelled by its parent process, close the cancel-chan
      ;; which will make cancel-chan produce `nil` and cause cancellation of read on `chan`.
      #(a/close! cancel-chan))))

(cc/defn chan->flow
  "Produces a discreet flow from a core.async `channel`"
  [channel]
  (m/ap ; returns a discreet flow
    (loop []
      (if-some [x (m/? (chan-read channel))] ; read one value from `channel`, waiting until `channel` produces it
        ;; We succesfully read a non-nil value, we use `m/amb` with two
        ;; branches. m/amb will fork the current process (ap) and do two things
        ;; sequencially, in two branches:
        ;; - return x, meaning `loop` ends and return x, ap will produce x
        ;; - recur to read the next value from chan
        (m/amb x (recur))
        ;; `channel` producing `nil` means it's been closed. We want to
        ;; terminate this flow without producing any value (not even nil), we
        ;; use (m/amb) which produces nothing and terminates immediately. The
        ;; parent m/ap block has nothing to produce anymore and will also
        ;; terminate.
        (m/amb)))))

(defmacro use-channel ;; TODO rename
  ([chan] `(use-channel nil ~chan))
  ([init chan] `(new (m/reductions {} ~init (chan->flow ~chan)))))

;; --------------------------------------

(defmacro def
  ([sym] `(hyperfiddle.photon/def ~sym ::c/unbound))
  ([sym form]
   ;; GG: Expand to an unbound var with body stored in ::c/node meta.
   ;;     Clojure compiler will analyze vars metas, which would analyze form as clojure, so we quote it.
   ;;     ClojureScript do not have vars at runtime and will not analyze or emit vars meta. No need to quote.
   `(def ~(vary-meta sym assoc ::c/node (if (:js-globals &env) form `(quote ~form))))))

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
    (->> (list ::c/lift v)
         (list `r/bind `map-by kf
               (->> body
                    (list* `for-by kf bindings)
                    (list `let [s (second c/arg-sym)])
                    (list `fn [])
                    (list `partial (list 'def (second c/arg-sym)))))
         (list `new))
    (cons `do body)))

(defmacro for [bindings & body]
  `(hyperfiddle.photon/for-by identity ~bindings ~@body))

(cc/defn ^:no-doc watchable? [x]
  #?(:clj (instance? clojure.lang.IRef x)
     :cljs (satisfies? IWatchable x)))

(cc/defn ^:no-doc checked-watch [!x]
  (assert (watchable? !x) "Provided argument is not Watchable.")
  (m/watch !x))

(defn Watch [!x]
  (new (checked-watch !x)))

(defmacro watch "for tutorials (to delay teaching constructor syntax); m/watch is also idiomatic"
  [!x] `(new (checked-watch ~!x)))

(cc/defn debounce-discreet
  ([delay flow] (debounce-discreet delay nil flow))
  ([delay init flow] (m/reductions {} init (m/ap (let [x (m/?< flow)]
                                                   (try (m/? (m/sleep delay x))
                                                        (catch Cancelled _ (m/amb))))))) )

(defmacro debounce
  "Debounce a continous flow by `delay` milliseconds."
  [delay flow]
  `(new (->> (fn [] ~flow)
             (debounce-discreet ~delay)
             (m/relieve {}))))
