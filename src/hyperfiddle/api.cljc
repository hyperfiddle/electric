(ns hyperfiddle.api
  (:require [contrib.expr :refer [quoted?]]
            [clojure.spec.alpha :as s]
            [hyperfiddle.rcf :refer [tests tap %]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.hfql :as hfql]
            [clojure.datafy :refer [datafy]]
            [hyperfiddle.spec :as spec])
  #?(:cljs (:require-macros [hyperfiddle.api :refer [hfql]])))

(defmacro hfql
  ([query] `(hfql/hfql ~query))
  ([bindings query] `(hfql/hfql ~bindings ~query)))

(p/def ^:dynamic *nav!*)

;;; Route

(p/def route nil) ; Continuous route value
(p/def navigate!) ; to inject a route setter (eg. write to url, html5 history pushState, swap an atom…)
(p/def replace-route!) ; overwrite the current route
(p/def navigate-back!) ; inverse of `navigate!`, to be injected

(defn empty-value? [x] (if (seqable? x) (empty? x) (some? x)))

(defn route-state->route [route-state]
  (if (= 1 (count route-state))
    (let [[k v] (first route-state)]
      (if (seq? k)
        (let [args (::spec/keys (datafy (spec/args (first k))))]
          (cons (first k) (map (fn [arg] (get v arg)) args)))
        (if (empty? v) nil route-state)))
    route-state))

(defn route-cleanup [m path]
  (cond
    (seq? m)      m
    (empty? path) m
    :else         (let [leaf (get-in m path)]
                    (cond
                      (empty-value? leaf) (if-some [path' (seq (butlast path))]
                                            (recur (update-in m path' dissoc (last path)) path')
                                            (route-state->route (reduce-kv (fn [r k v] (if (and (not (seq? k)) (empty? v)) (dissoc r k) r)) m m)))
                      :else               m))))

(defn assoc-in-route-state [m path value]
  (let [empty? (if (seqable? value) (not-empty value) (some? value))]
    (if empty?
      (assoc-in m path value)
      (route-cleanup (assoc-in m path value) path))))

;;; Database

(def db-state #?(:clj (atom nil))) ; Server side only
(p/def db-name)

(def ^:dynamic *$*) ; dbval, for REPL usage. Available in cljs for HFQL datascript tests
(p/def db) ; HFQL will query this db

(p/def ^{:dynamic true, :doc "To be bound to a function [db attribute] -> schema"} *schema*)
(p/def ^{:dynamic true, :doc "To be bound to a function schema -> ::hf/one | ::hf/many"} *cardinality*
  (fn cardinality [schemaf db attr]
    (let [card
          ({:db.cardinality/one ::one
            :db.cardinality/many ::many} (:db/cardinality (schemaf db attr)))]
      card)))

(p/def context nil) ; HFQL EAV context

(p/defn entity []) ;; TODO HFQL only. Is a binding required? could it be an argument?
(p/def ^:deprecated attribute nil)      ; TODO G: not sure if actually deprecated.
(p/def value (p/fn [] nil))
(p/def options nil)

(p/def scope)

(p/defn ^:no-doc ^:deprecated FanOut [Continuation value props]
  (binding [hyperfiddle.api/value (p/fn [] (p/for [e value] (p/fn [] (Continuation. e))))]
    (Render. hyperfiddle.api/value props)))

(p/defn tx [v' props]
  (if-let [Txfn (::tx props)]
    (Txfn. v')
    (when v'
      (let [[E a _] (first context)]
        [[:db/add (E.) a v']]))))

(defmulti tx-meta (fn [schema tx] (if (map? tx) ::map (first tx))))

(s/def ::tx-cardinality (s/or :one :many))
(s/def ::tx-identifier map?)
(s/def ::tx-inverse fn?)
(s/def ::tx-special fn?)
(s/def ::transaction-meta (s/keys :req [::tx-identifier]
                                  :opt [::tx-cardinality ::tx-inverse ::tx-special
                                        ::tx-conflicting?]))
(s/fdef tx-meta :ret ::transaction-meta)

(p/defn Join-all "Given a collection of flows, join all flows. Maps are expected to be {Key Flow<Value>}."
  [v]
  (cond
    (reduced? v) (unreduced v)
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
      (Renderer. (p/fn [] (unreduced (V.))) props)
      (Join-all. (V.)))))

(p/defn Data [V] (binding [Render (p/fn [V _props] (let [v (V.)] #_(prn "hf/data" v) (Join-all. v)))] (Render. V nil)))

(def ^:dynamic *http-request* "Bound to the HTTP request of the page in which the current photon program is running." nil)

;; TODO Rename, this is not Haskell traverse, name is confusing.
(p/defn Traverse [F V]
  (new (p/Y. (p/fn [Rec]
               (p/fn [V]
                 ;; (prn (:dbg/name (meta V)))
                 (let [{::keys [columns]} (meta V)]
                   (F. V (p/fn [v]
                           (cond
                             (fn? v)     (Rec. v)
                             (map? v)    (into {} (p/for [col columns] [col (Rec. (get v col))]))
                             (vector? v) (p/for [V v] (Rec. V))
                             :else       (prn "unreachable"))))))))
    V))

;; TODO Rename
(p/defn JoinAllTheTree "Join all the tree, does not call renderers, return EDN." [V]
  (new (p/Y. (p/fn [Rec]
               (p/fn [V]
                 (let [{::keys [render cardinality columns leaf?]} (meta V)
                       v (V.)]
                   ;; (prn "> " (meta V) v)
                   (case cardinality
                     ::many (p/for [V v] (Rec. V))
                     ;; infer
                     (cond
                       leaf?       v
                       (vector? v) (p/for [V v] (Rec. V))
                       (map? v)    (into {} (p/for [col columns] [col (Rec. (get v col))]))
                       :else       v))))))
    V))

;; TODO Rename, this seems to just be "Render"
(p/defn EdnRender "Join all the tree, calling renderers when provided, return EDN" [V]
  (new (p/Y. (p/fn [Rec]
               (p/fn [V]
                 (let [{::keys [render cardinality columns leaf?]} (meta V)]
                   (if render (render. V )
                     (let [v (V.)]
                       ;; (prn "> " #_(meta V) v)
                       (case cardinality
                         ::many (p/for [V v] (Rec. V))
                         ;; infer
                         (cond
                           leaf?       v
                           (vector? v) (p/for [V v] (Rec. V))
                           (map? v)    (into {} (p/for [col columns] [col (Rec. (get v col))]))
                           :else       v))))))))
    V))

(p/defn Sequence ":: t m a -> m t a" [Vs] (p/fn [] (p/for [V Vs] (V.))))

(p/defn TreeToRows [V]
  (new (p/Y. (p/fn [Rec]
               (p/fn [[V depth]]
                 (let [{::keys [continuation columns render] :as m} (meta V)
                       v (if render (render. V) (V.))]
                   (if-not continuation
                     [(p/fn [] [depth (p/fn [] [(p/fn [] v)])])]
                     (cond
                       (fn? v)     (Rec. [v depth])
                       (map? v)    (into [] cat (p/for [col columns]
                                                  (let [v' (get v col)]
                                                    (if (::continuation (meta v'))
                                                      (into [(p/fn [] [depth
                                                                       (p/fn []
                                                                         [(p/fn []
                                                                            col)])])]
                                                        (Rec. [v' (inc depth)]))
                                                      [(p/fn [] [depth
                                                                 (p/fn []
                                                                   [(p/fn [] col)
                                                                    (p/fn [] (if-let [render (::render (meta v'))]
                                                                               (render. v')
                                                                               (new v')))])])]))))
                       (vector? v) (into [] cat (p/for [V v] (Rec. [V (inc depth)])))))))))
    [V 0]))
