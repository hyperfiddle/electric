(ns dustin.cloroutine2
  (:require
    [cloroutine.core :refer [cr]]
    [cloroutine.impl :as i]
    [minitest :refer [tests]])
  (:import [java.util.concurrent CompletableFuture]))

(tests
  "java stuff prerequisites"
  (def x (reify java.util.function.BiConsumer
           (accept [this v e]
             (println 'accept v e)
             ::done)))
  (.accept x 1 2) := nil                                   ; for effect
  )

(def ^:dynamic *fiber*)
(def ^:dynamic *value*)
(def ^:dynamic *error*)

(defn await [cf] (.whenComplete cf *fiber*))
(defn thunk [] (println 'thunk *value*) *value*)

(defmacro async [& body]
  `(let [cf# (CompletableFuture.)
         cr# (cr {await thunk}
               (.complete cf# (do ~@body)))]
     (binding [*fiber* (reify java.util.function.BiConsumer
                         (accept [this# v# e#]
                           (binding [*fiber* this#
                                     *value* v#]
                             (println 'accept)
                             (cr#))))]
       (cr#))
     cf#))

(defmethod ana/macroexpand-hook `cloroutine.core/cr [_the-var _form _env args] `(cloroutine.core/cr ~@args))

(tests
  (def six (async 6))
  (def seven (async (inc (await six))))
  )
