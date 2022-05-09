(ns hyperfiddle.api
  (:require #?(:clj [datahike.api :as d]
               :cljs [datascript.core :as d])
            #?(:clj [datahike.impl.entity :as de]
               :cljs [datascript.impl.entity :as de])
            [hyperfiddle.rcf :refer [tests ! %]]
            [hyperfiddle.photon :as p :refer [vars]]
            [missionary.core :as m]
            [hyperfiddle.hfql :as hfql]
            #?(:clj [hyperfiddle.dev.logger :as log]))
  #?(:cljs (:require [hyperfiddle.dev.logger :as log]))
  #?(:cljs (:require-macros [hyperfiddle.api :refer [route db entity attribute value context refs props sequenceM render join-all data tx
                                                     ]])))

(defmacro hfql [& body] `(hfql/hfql ~@body))

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

(p/def route)

(def db-state #?(:clj (atom nil)))

(defrecord DB [name basis-t tempids db])
(p/def db nil)
(def ^:dynamic *db*)
(def ^:dynamic *$*)                                         ; available in cljs for HFQL datascript tests

(p/def context nil)

(p/defn entity [])
(p/def ^:deprecated attribute nil)
(p/defn ^:deprecated value [])
(p/defn ^:deprecated options-attribute [])


(p/defn tx [v' props]
  (if-let [Txfn (::tx props)]
    (Txfn. v')
    (when v'
      (let [[E a _] (first context)]
        [[:db/add (E.) a v']]))))

(p/def ^:deprecated refs {}) ;; reference points in HFQL expr
(p/def ^:deprecated columns [])
(p/def ^:deprecated inputs [])

(defn quoted? [form]
  (and (seq? form)
       (= 'quote (first form))))

(p/defn join-all [v]
  (cond
    (quoted? v) v
    (map? v)    (into {} (p/for [[k V] v] [k (V.)]))
    (list? v)   (p/for [V v] (V.))
    (coll? v)   (into (empty v) (p/for [V v] (V.)))
    :else       v))

;; https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad.html#v:sequence
(p/defn sequenceM [V _props] (join-all. (V.)))

(p/defn render [V props]
  (if-let [Renderer (::render props)]
    (Renderer. V props)
    (if-let [link (::link props)]
      (str "<a href=\"" (pr-str (new (second link))) "\">" (join-all. (V.)) "</a>")
      (join-all. (V.)))))

(p/defn data [V] (binding [render sequenceM] (render. V nil)))


(tests
 (p/run (! (join-all. [(p/fn [] 1) (p/fn [] 2) (p/fn [] 3)])))
 % := [1 2 3])

(tests
 (p/run (! (join-all. (list (p/fn [] 1) (p/fn [] 2) (p/fn [] 3)))))
 % := '(1 2 3))

(tests
 (p/run (! (join-all. {:a (p/fn [] 1), :b (p/fn [] 2), :c (p/fn [] 3)})))
 % := '{:a 1, :b 2, :c 3})

(p/def ^:deprecated props {})
(p/def ^:deprecated args {})

; todo rename wrap, it's sideeffect-fn to fn-returning-flow
(defn wrap [f] (fn [& args] #?(:clj (m/ap #_(m/? (m/via m/blk (apply f args))) ;; TODO restore and handle continuous flow initial state
                                     (apply f args))
                               :cljs (m/ap (apply f args))))) ; m/via not supported in cljs

(def q (wrap (fn [query & args]
               (log/debug :q query args)
               (doto (apply d/q query args) log/debug))))

(tests
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

(tests
 (nav! *$* 9 :order/email)
 := "alice@example.com"

 (m/? (m/reduce conj (nav *$* 9 :order/email)))
 := ["alice@example.com"])

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
;(def datalog-rules '[[(match-str ?v ?needle) [(match-str ?v ?needle)]]])

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

