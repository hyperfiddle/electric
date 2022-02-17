(ns hyperfiddle.q8.lib
  (:require [missionary.core :as m]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf])
  #?(:clj (:import (clojure.lang IRef IDeref IAtom IFn)))
  #?(:cljs (:require-macros [hyperfiddle.q8.lib :refer [render read-only? set-ref!]])))

;; Both Atom and m/watch, ;; Atom is final, can’t proxy it.
#?(:clj (deftype Reference [^clojure.lang.IRef !atom watch]
          clojure.lang.IRef
          (setValidator [_this vf] (set-validator! !atom vf))
          (getValidator [_this] (get-validator !atom))
          (getWatches [_this] (.getWatches !atom))
          (addWatch [_this key callback] (add-watch !atom key callback))
          (removeWatch [_this key] (remove-watch !atom key))
          clojure.lang.IDeref
          (deref [_this] (deref !atom))
          clojure.lang.IAtom ;; TODO IAtom2
          (swap [_this f] (swap! !atom f))
          (swap [_this f & args] (apply swap! !atom f args))
          (compareAndSet [_this oldv newv] (compare-and-set! !atom oldv newv))
          (reset [_this newval] (reset! !atom newval))

          clojure.lang.IFn
          (invoke [_this notify terminate] (watch notify terminate)))
   :cljs (deftype Reference [^clojure.lang.IRef !atom watch] ;; TODO incomplete
           ;; see `https://github.com/clojure/clojurescript/blob/r1.10.914/src/main/cljs/cljs/core.cljs#L4453`
           IAtom
           IWatchable
           (-add-watch [_this key callback] (add-watch !atom key callback))
           (-remove-watch [_this key] (remove-watch !atom key))
           IDeref
           (-deref [_this] (deref !atom))

           IFn
           (-invoke [_this notify terminate] (watch notify terminate))))

(defn reference [init]
  (let [!atom (atom init)]
    (Reference. !atom (m/watch !atom))))

(p/def read-only? false)

(p/defn set-ref! [k v]
  (when-not read-only?
    (let [ref (get hf/refs k)]
      (reset! ref v)))
  v)

;; FAIL can’t use `hf/props` because the sym will be stored in `:node` and runtime will resolve it in a namespace where hf alias is unknown
(p/def render #'(if-let [renderer (::hf/render hyperfiddle.api/props)]
                  (do #_(assert (fn? renderer) "::hf/render must be a reactive function") ;; FIXME macroexpand to constructor call, not supported by photon yet.
                      ~renderer)
                  ~hyperfiddle.api/render))
