(ns hyperfiddle.api
  (:require #?(:clj [datahike.api :as d]
               :cljs [datascript.core :as d])
            #?(:clj [datahike.impl.entity :as de]
               :cljs [datascript.impl.entity :as de])
            [hyperfiddle.rcf :refer [tests ! %]]
            [hyperfiddle.photon :as p]
            [missionary.core :as m]
            [hyperfiddle.hfql :as hfql]
            #?(:clj [hyperfiddle.logger :as log]))
  #?(:cljs (:require [hyperfiddle.logger :as log]))
  #?(:cljs (:require-macros [hyperfiddle.api :refer [hfql route db entity attribute context tx]])))

(defmacro hfql [& body] `(hfql/hfql ~@body))

;;; Route

(def route-state #?(:cljs (atom ()))) ;; Route state lives on the client.

(defn- pad [val n coll] (into coll (repeat n val)))

(defn- set-route-arg [index val route]
  (seq (let [route (vec route)]
         (if (<= index (count route))
           (assoc route index val)
           (set-route-arg index val (pad nil (- index (count route)) route))))))

(defn set-route-arg! [index val] (swap! route-state (fn [[current & history]]
                                                      (cons (set-route-arg index val current) history))))

(defn navigate! [new-route] (swap! route-state conj new-route))

(defn navigate-back! [] (swap! route-state rest))

(p/def route) ;; Continuous route value

;;; Database

(def db-state #?(:clj (atom nil))) ; Server side only
(p/def db nil) ;; Continuous db value

(defrecord DB [name basis-t tempids db]) ; Full db description (name, time basis, tempids map, dbval)
(def ^:dynamic *db*) ; hold current db description, for REPL usage.
(def ^:dynamic *$*) ; dbval, for REPL usage. Available in cljs for HFQL datascript tests
                                      

(p/def context nil) ; HFQL EAV context

(p/defn entity []) ;; TODO HFQL only. Is a binding required? could it be an argument?
(p/def ^:deprecated attribute nil)


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
  (p/run (! (Join-all. [(p/fn [] 1) (p/fn [] 2) (p/fn [] 3)])))
  % := [1 2 3])

(tests
  (p/run (! (Join-all. (list (p/fn [] 1) (p/fn [] 2) (p/fn [] 3)))))
  % := '(1 2 3))

(tests
  (p/run (! (Join-all. {:a (p/fn [] 1), :b (p/fn [] 2), :c (p/fn [] 3)})))
  % := '{:a 1, :b 2, :c 3})

(declare Render)

(p/defn Render [V props]
  (if-let [Renderer (::render props)]
    (Renderer. V props)
    (Join-all. (V.))))

(p/defn Data [V] (binding [Render (p/fn [V _props] (Join-all. (V.)))] (Render. V)))

; todo rename wrap, it's sideeffect-fn to fn-returning-flow
(defn wrap [f] (fn [& args] #?(:clj (m/ap #_(m/? (m/via m/blk (apply f args))) ;; TODO restore and handle continuous flow initial state
                                     (apply f args))
                               :cljs (m/ap (apply f args))))) ; m/via not supported in cljs

(def q (wrap (fn [query & args]
               (log/debug :q query args)
               (doto (apply d/q query args) log/debug))))

(comment
  ; no dev *$* yet, and RCF might be enabled
 (d/q '[:find [?e ...] :where [_ :order/gender ?g] [?g :db/ident ?e]] *$*)
 := [:order/female :order/male]
 (m/? (m/reduce conj (q '[:find [?e ...] :where [_ :order/gender ?g] [?g :db/ident ?e]] *$*)))
 := [[:order/female :order/male]])

(defn nav!
  ([_ e] e)
  ([db e a] (let [v (a (if (de/entity? e) e (d/entity db e)))]
              (if (de/entity? v)
                (:db/id v)
                v)))
  ([db e a & as] (reduce (partial nav! db) (nav! db e a) as)))

(def nav (wrap (fn [db e a]
                 (log/debug :nav e a)
                 (doto (nav! db e a) log/debug))))

(comment
 ; no dev db yet, and RCF might be enabled
 (nav! *$* 9 :order/email)
 := "alice@example.com"

 (m/? (m/reduce conj (nav *$* 9 :order/email)))
 := ["alice@example.com"])


#?(:clj (defn transact!
          ([db stage] (transact! db stage false))
          ([db stage commit?]
           (try (let [db'
                      (if commit?
                        (throw (ex-info "Commit not implemented yet" {}))
                        (if (not-empty stage)
                          (let [{:keys [tempids db-after]} (d/with (:db db) {:tx-data stage})]
                            (-> (assoc db :tempids tempids :db db-after)
                                (update :basis-t (fnil inc 0))))
                          db))]
                  #_(prn "DB =>" db')
                  #_(prn "stage =>" stage)
                  [db' nil])
                (catch Throwable t
                  [db (ex-message t)]))))
   :cljs (defn transact! [& _] (throw (ex-info "Server side only" {}))))

(def rules
  '[[(hyperfiddle.api/needle-match ?v ?needle)
     [(str ?v) ?v']
     [(str ?needle) ?needle']
     #_[(.toLowerCase ?v')]
     #_[(.toLowerCase ?needle')]
     #_[(clojure.string/includes? ?v' ?needle')]
     [(clojure.string/includes? ?v' ?needle')]]])

(defn includes-str? [v needle]
  (clojure.string/includes? (.toLowerCase (str v))
    (.toLowerCase (str needle))))