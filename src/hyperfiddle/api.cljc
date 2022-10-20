(ns hyperfiddle.api
  (:require #?(:clj [datascript.core :as d])
            [hyperfiddle.rcf :refer [tests tap %]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.hfql :as hfql]
            [clojure.datafy :refer [datafy]]
            [hyperfiddle.spec :as spec])
  #?(:cljs (:require-macros [hyperfiddle.api :refer [hfql]])))

(defmacro hfql [& body] `(hfql/hfql ~@body))
(def nav! #'hfql/nav!)

;;; Route

(p/def route) ; Continuous route value
(p/def navigate!) ; to inject a route setter (eg. write to url, html5 history pushState, swap an atom…)
(p/def replace-route!) ; overwrite the current route
(p/def navigate-back!) ; inverse of `navigate!`, to be injected

(defn empty-value? [x] (if (seqable? x) (empty? x) (some? x)))

(defn route-state->route [route-state]
  (when-let [[k v] (first route-state)]
    (let [args (::spec/keys (datafy (spec/args (first k))))]
      (cons (first k) (map (fn [arg] (get v arg)) args)))))

(defn route-cleanup [m path]
  (cond
    (seq? m)      m
    (empty? path) m
    :else         (let [leaf (get-in m path)]
                    (cond
                      (empty-value? leaf) (if-some [path' (seq (butlast path))]
                                            (recur (update-in m path' dissoc (last path)) path')
                                            (route-state->route m))
                      :else               m))))

(defn assoc-in-route-state [m path value]
  (let [empty? (if (seqable? value) (not-empty value) (some? value))]
    (if empty?
      (assoc-in m path value)
      (route-cleanup (assoc-in m path value) path))))

;;; Database

(def db-state #?(:clj (atom nil))) ; Server side only
(p/def db nil) ;; Continuous db value
(p/def db-name)

(defrecord DB [name basis-t tempids db]) ; Full db description (name, time basis, tempids map, dbval)
(def ^:dynamic *db*) ; hold current db description, for REPL usage.
(def ^:dynamic *$*) ; dbval, for REPL usage. Available in cljs for HFQL datascript tests
                                      

(p/def context nil) ; HFQL EAV context

(p/defn entity []) ;; TODO HFQL only. Is a binding required? could it be an argument?
(p/def ^:deprecated attribute nil)      ; TODO G: not sure if actually deprecated.
(p/def value (p/fn [] nil))
(p/def options)

(p/defn tx [v' props]
  (if-let [Txfn (::tx props)]
    (Txfn. v')
    (when v'
      (let [[E a _] (first context)]
        [[:db/add (E.) a v']]))))

(defn quoted? [form]
  (and (seq? form)
       (= 'quote (first form))))

(p/defn Join-all "Given a collection of flows, join all flows. Maps are expected to be {Key Flow<Value>}."
  [v]
  (cond
    (quoted? v) v
    (map? v)    (into {} (p/for [[k V] v] [k (V.)]))
    (list? v)   (p/for [V v] (V.))
    (coll? v)   (into (empty v) (p/for [V v] (V.)))
    :else       v))

(tests
  (p/run (tap (Join-all. [(p/fn [] 1) (p/fn [] 2) (p/fn [] 3)])))
  % := [1 2 3])

(tests
  (p/run (tap (Join-all. (list (p/fn [] 1) (p/fn [] 2) (p/fn [] 3)))))
  % := '(1 2 3))

(tests
  (p/run (tap (Join-all. {:a (p/fn [] 1), :b (p/fn [] 2), :c (p/fn [] 3)})))
  % := '{:a 1, :b 2, :c 3})

(p/def bypass-renderer false) ; hf/options bypases propgressive enhancement to produces EDN

(p/defn Render [V props]
  (if bypass-renderer
    (Join-all. (V.))
    (if-let [Renderer (::render props)]
      (Renderer. V props)
      (Join-all. (V.)))))

(p/defn Data [V] (binding [Render (p/fn [V _props] (let [v (V.)] #_(prn "hf/data" v) (Join-all. v)))] (Render. V nil)))

(def ^:dynamic *http-request* "Bound to the HTTP request of the page in which the current photon program is running." nil)
