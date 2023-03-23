(ns contrib.trace
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.rcf :as rcf :refer [% tap tests with]])
  (:import
   [hyperfiddle.electric Pending]
   [missionary Cancelled])
  #?(:cljs (:require-macros contrib.trace)))

(e/def listeners '())
(defmacro with-listener [l & body] `(binding [listeners (conj listeners ~l)] ~@body))
(e/def stamp (fn [_]))
(e/def ->point-id (fn [_name _parent-id]))
(e/def ->trace-id (fn [_point-id _v]))
(e/def current nil)

;; https://github.com/cgrand/macrovich/blob/e80fb37cb795201821d0e75f73119802227e9620/src/net/cgrand/macrovich.cljc
(defmacro macro-case [& {:keys [cljs clj]}]
  (if (contains? &env '&env)
    `(if (:ns ~'&env) ~cljs ~clj)
    (if #?(:clj (:ns &env) :cljs true) cljs clj)))

(defmacro trace [nm & body]
  `(let [nm# ~nm, id# (->point-id nm# (or current 0)), point# {::point-id id#, ::name nm#, ::parent (or current ::root)}]
     (doseq [l# listeners] (l# point#))
     (let [[typ# v#] (binding [current id#]
                       (try [::ok (do ~@body)]
                            (catch ~(macro-case :clj Throwable :cljs :default) e# [::err e#])))
           trace-id# (->trace-id id# v#)
           stamp# (stamp v#)
           ;; TODO put exception in trace map
           tr# {::trace-id trace-id#, ::stamp stamp#, ::type typ#, ::v v#}]
       (doseq [l# listeners] (l# id# tr#))
       (case typ# ::ok v# #_else (throw v#)))))

(defmacro trace* [nm & body] `(when current (trace ~nm ~@body)))

(defn- test-listener [tap]
  (fn
    ([point]    (tap [:point point]))
    ([id trace] (tap [:trace id trace]))))

(tests "basic behavior"
  (with (e/run (try (binding [->point-id #(do (tap [:point-id % %2]) %)
                              stamp (let [!t (atom 0)] (fn [_] (let [t (swap! !t inc)] (tap [:stamp]) t)))
                              ->trace-id (fn [id v] (tap [:trace-id id v]) (str (name id) "-1"))]
                      (with-listener (test-listener tap)
                        (tap (trace :outer
                               (trace :inner 0 1)))))
                    (catch Pending _)
                    (catch Cancelled _)
                    (catch #?(:clj Throwable :cljs :default) e (prn :error (ex-message e)) (throw e))))
    % := [:point-id :outer 0]
    % := [:point {::point-id :outer, ::name :outer, ::parent ::root}]
    % := [:point-id :inner :outer]
    % := [:point {::point-id :inner, ::name :inner, ::parent :outer}]
    % := [:trace-id :inner 1]
    % := [:stamp]
    % := [:trace :inner {::trace-id "inner-1", ::stamp 1, ::v 1, ::type ::ok}]
    % := [:trace-id :outer 1]
    % := [:stamp]
    % := [:trace :outer {::trace-id "outer-1" ::stamp 2, ::v 1, ::type ::ok}]
    % := 1))

(defn monotonic [] (let [!t (atom 0)] (fn [_] (swap! !t inc))))

(tests "exceptions"
  (with (e/run (try (binding [->point-id (fn [nm _parent] nm), stamp (monotonic), ->trace-id (fn [id _v] (str (name id) "-1"))]
                      (with-listener (test-listener tap)
                        (tap (trace :x (throw (ex-info "boom" {}))))))
                    (catch #?(:clj Throwable :cljs :default) e
                      (when-not (= "boom" (ex-message e)) (prn [(type e) (ex-message e)])))))
    % := [:point {::point-id :x, ::name :x, ::parent ::root}]
    % := [:trace :x {::trace-id "x-1" , ::stamp 1, ::v _, ::type ::err}]))

(tests "works across e/fn boundaries"
  (with (e/run (try (binding [->point-id (fn [nm _parent] nm), stamp (monotonic), ->trace-id (fn [id _v] (str (name id) "-1"))]
                      (with-listener (test-listener tap)
                        (trace :outer
                          (new (e/fn [] (trace :inner 1))))))
                    (catch Pending _)
                    (catch Cancelled _)
                    (catch #?(:clj Throwable :cljs :default) e (prn :error (ex-message e)) (throw e))))
    % := [:point _]
    % := [:point _]
    % := [:trace :inner {::trace-id "inner-1" ::stamp 1, ::v 1, ::type ::ok}]
    % := [:trace :outer {::trace-id "outer-1", ::stamp 2, ::v 1, ::type ::ok}]))
