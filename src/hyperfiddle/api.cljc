(ns hyperfiddle.api
  (:require [clojure.datafy :refer [datafy]]
            clojure.edn
            [contrib.dynamic :refer [call-sym]]
            [contrib.data :as data]
            [clojure.spec.alpha :as s]
            [hyperfiddle.hfql :as hfql]
            [hyperfiddle.photon :as p]
            [missionary.core :as m]
            hyperfiddle.photon-dom2
            [hyperfiddle.spec :as spec]
            [hyperfiddle.rcf :refer [tests]])
  (:import [hyperfiddle.photon Pending]
           #?(:cljs [goog.math Long]))
  #?(:cljs (:require-macros [hyperfiddle.api :refer [hfql]])))

(def ^:dynamic *$*) ; dbval, for REPL usage. Available in cljs for HFQL datascript tests
(p/def db "inject database value for hyperfiddle stage and HFQL")
(s/def ::ref? any?)
(p/def secure-db "database value excluding stage, so that user can't tamper")
(p/def with "inject datomic.api/with or equivalent, used by stage")
(p/def into-tx')
(def -read-edn-str-default (partial clojure.edn/read-string
                                    {:readers #?(:cljs {'goog.math/Long goog.math.Long/fromString} ; datomic cloud long ids
                                                :clj {})}))
(p/def read-edn-str "inject app-specific edn extensions" -read-edn-str-default) ; avoid photon warning about goog.math.Long
(p/def ^:dynamic *nav!*)

(defmacro hfql ; Alias
  ([query] `(hfql/hfql ~query))
  ([bindings query] `(hfql/hfql ~bindings ~query))
  ([bindings query eid] `(hfql/hfql ~bindings ~query ~eid)))

(p/def Render hfql/Render)

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

(defn entity [ctx] (or (::entity ctx) (::entity (::parent ctx))))
(defn attribute [ctx] (or (::attribute ctx) (::attribute (::parent ctx))))

(p/def validation-hints nil)

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
  (new (p/task->cp
         ;; workaround: Datomic doesn't handle a thread interrupt correctly
         (m/compel
           (m/via m/blk
             ;; return basis-t ?
             (swap! !t (fn [[db tx0]]
                         [(with db tx) ; injected datomic dep
                          (into-tx' schema tx0 tx)]))))))) ; datascript is different

(p/def Transact!) ; server
(p/def stage) ; server
(p/def loading) ; client

(p/defn Load-timer []
  (p/client
    (let [[x] (p/with-cycle [[elapsed start :as s] [0 nil]]
                (case hyperfiddle.api/loading
                  true [(some->> start (- hyperfiddle.photon-dom2/system-time-ms))
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
                                            #_(println "Transact! " (hash !t) "committing: " tx)
                                            (let [r (Transact!*. !t tx)]
                                              #_(println "Transact! " (hash !t) "commit result: " r)))]
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

(p/def page-drop -1)
(p/def page-take -1)

(p/defn Paginate [xs]
  (if (coll? xs)
    (cond->> xs
      (pos-int? page-drop) (drop page-drop)
      (pos-int? page-take) (take page-take))
    xs))
