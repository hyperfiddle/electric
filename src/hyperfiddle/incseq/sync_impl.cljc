(ns hyperfiddle.incseq.sync-impl
  #?(:cljs (:require-macros [hyperfiddle.incseq.sync-impl])))

(defmacro get-sync [obj slot]
  (if (:js-globals &env)
    `(aget ~obj ~slot)
    `(.get ^java.util.concurrent.atomic.AtomicReference (aget ~obj ~slot))))

(defmacro cas-sync [obj slot prev curr then else]
  (if (:js-globals &env)
    `(if (identical? ~prev (aget ~obj ~slot)) (do (aset ~obj ~slot ~curr) ~then) ~else)
    `(if (.compareAndSet ^java.util.concurrent.atomic.AtomicReference (aget ~obj ~slot) ~prev ~curr) ~then ~else)))
