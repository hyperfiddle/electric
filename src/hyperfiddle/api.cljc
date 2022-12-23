(ns hyperfiddle.api
  (:require [clojure.datafy :refer [datafy]]
            clojure.edn
            [contrib.dynamic :refer [call-sym]]
            [clojure.spec.alpha :as s]
            [hyperfiddle.hfql :as hfql]
            [hyperfiddle.photon :as p]
            hyperfiddle.photon-dom
            [hyperfiddle.spec :as spec])
  (:import [hyperfiddle.photon Pending]
           #?(:cljs [goog.math Long]))
  #?(:cljs (:require-macros [hyperfiddle.api :refer [hfql]])))

(def ^:dynamic *$*) ; dbval, for REPL usage. Available in cljs for HFQL datascript tests
(p/def db "inject database value for hyperfiddle stage and HFQL")
(declare ref?)
(p/def secure-db "database value excluding stage, so that user can't tamper")
(p/def with "inject datomic.api/with or equivalent, used by stage")
(def -read-edn-str-default (partial clojure.edn/read-string
                                    {:readers #?(:cljs {'goog.math/Long goog.math.Long/fromString} ; datomic cloud long ids
                                                :clj {})}))
(p/def read-edn-str "inject app-specific edn extensions" -read-edn-str-default) ; avoid photon warning about goog.math.Long
(p/def ^:dynamic *nav!*)

(defmacro hfql ; Alias
  ([query] `(hfql/hfql ~query))
  ([bindings query] `(hfql/hfql ~bindings ~query)))

(p/def Render hfql/Render)

(p/def route nil) ; Continuous route value
(p/def navigate!) ; to inject a route setter (eg. write to url, html5 history pushState, swap an atom…)
(p/def replace-route!) ; overwrite the current route
(p/def navigate-back!) ; inverse of `navigate!`, to be injected

(defn empty-value? [x] (if (seqable? x) (empty? x) (some? x)))

;; FIXME decomplect route from route state
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

;; FIXME decomplect route from route state
(defn assoc-in-route-state [m path value]
  (let [empty? (if (seqable? value) (not-empty value) (some? value))
        m      (if (or (seq? m) (vector? m)) {} m)]
    (if empty?
      (assoc-in m path value)
      (route-cleanup (assoc-in m path value) path))))

;;; Database

(def db-state #?(:clj (atom nil))) ; Server side only
(p/def db-name)

(p/def schema "pre-fetched schema for explorer")
(p/def ^{:dynamic true, :doc "To be bound to a function [db attribute] -> schema"} *schema*)
(p/def ^{:dynamic true, :doc "To be bound to a function schema -> ::hf/one | ::hf/many"} *cardinality*
  (fn cardinality [schemaf db attr]
    (let [card
          ({:db.cardinality/one ::one
            :db.cardinality/many ::many} (:db/cardinality (schemaf db attr)))]
      card)))

(p/defn entity []) ;; TODO HFQL only. Is a binding required? could it be an argument?

(p/defn Link [args] args) ; inject how HFQL should generate a Link

(p/defn tx "WIP, this default impl captures the essence" [v' props] ; meant to be called by a renderer
  ;; Does it return a tx or side-effect to the staging area?
  (assert false "TBD")
  #_(if-let [Txfn (::tx props)] ; provided by hfql (props … {::hf/tx (p/fn [] …)})
      (Txfn. v')
      (when v'
        (let [[E a _] (first context)] ; context is a stack of [[E a] …] in dynamic scope ; MISSING today
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

; resolve cycle - hyperfiddle.txn needs hf/tx-meta
#?(:clj (require 'hyperfiddle.txn)) ; [rosie] before rcf turns on due to test/seattle undefined
#?(:clj (defn expand-hf-tx [tx] (call-sym 'hyperfiddle.txn/expand-hf-tx tx)))
;#?(:clj
;   (defmacro into-tx
;     ([tx tx'] `(call-sym ~'hyperfiddle.txn/into-tx ~hyperfiddle.api/schema ~tx ~tx')) ; photon call can infer schema
;     ([schema tx tx'] `(call-sym ~'hyperfiddle.txn/into-tx ~schema ~tx ~tx'))) ; clojure compatible call
;   :cljs (def into-tx nil))
#?(:clj (defn into-tx
          ;([tx tx'] (into-tx schema tx tx')) -- needs photon->clojure binding conveyance
          ([schema tx tx'] (call-sym 'hyperfiddle.txn/into-tx schema tx tx'))))

(p/defn Transact!* [!t tx] ; colorless, !t on server
  ; need the flattening be atomic?
  #_(when-some [tx (seq (hyperfiddle.txn/minimal-tx hyperfiddle.api/db tx))]) ; stabilize first loop (optional)
  (p/wrap
    ; return basis-t ?
    (swap! !t (fn [[db tx0]]
                [(with db tx) ; injected datomic dep
                 (into-tx schema tx0 tx)]))))

(p/def Transact!) ; server
(p/def stage) ; server
(p/def loading) ; client

(p/defn Load-timer []
  (p/client
    (let [[x] (p/with-cycle [[elapsed start :as s] [0 nil]]
                (case hyperfiddle.api/loading
                  ::loading [(some->> start (- hyperfiddle.photon-dom/system-time-ms))
                             (js/Date.now)]
                  s))]
      x)))

(p/defn Branch [Body-server] ; todo colorless
  (p/server
    (let [!ret (atom nil)
          !t (atom #_::unknown [db []])
          [db stage] (p/watch !t)]
      (binding [hyperfiddle.api/db db
                hyperfiddle.api/stage stage
                hyperfiddle.api/Transact! (p/fn [tx]
                                            (println "Transact! " (hash !t) "committing: " tx)
                                            (let [r (Transact!*. !t tx)]
                                              (println "Transact! " (hash !t) "commit result: " r)
                                              r))]
        (p/client
          (p/with-cycle [loading false]
            (binding [hyperfiddle.api/loading loading]
              #_(dom/div (name loading) " " (str (Load-timer.)) "ms")
              (try
                (p/server
                  (let [x (Body-server.)] ; cycle x?
                    #_(println 'Branch x)
                    (reset! !ret x))) ; if the body returns something, return it. (Likely not used)
                false (catch Pending e true))))
          nil))
      (p/watch !ret)))) ; do we need this? Popover using it currently

(defmacro branch [& body] `(new Branch (p/fn [] ~@body)))

(def ^:dynamic *http-request* "Bound to the HTTP request of the page in which the current photon program is running." nil)
