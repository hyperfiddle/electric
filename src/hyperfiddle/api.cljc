(ns hyperfiddle.api
  (:require #?(:clj [datahike.api :as d]
               :cljs [datascript.core :as d])
            #?(:clj [datahike.impl.entity :as de]
               :cljs [datascript.impl.entity :as de])
            [hyperfiddle.rcf :refer [tests ! %]]
            [hfdl.lang :as p :refer [vars]]
            [missionary.core :as m]
            #?(:clj [hyperfiddle.dev.logger :as log]))
  #?(:cljs (:require [hyperfiddle.dev.logger :as log]))
  #?(:cljs (:require-macros [hyperfiddle.api :refer [route db entity attribute value context refs props sequenceM render join-all data tx
                                                     ]])))

;; TODO remove, use hf/db instead
(def ^:dynamic *$*)                                         ; available in cljs for HFQL datascript tests

(defrecord DB [name basis-t tempids db])

(def route-state #?(:cljs (atom nil))) ;; Route state lives on the client.

(p/defn set-route! [new-route] ;; could be rebound to set the URL.
  (reset! route-state new-route))

(p/def route)

(def db-state #?(:clj (atom nil)))

(p/def db nil)

(p/def context nil)

(p/def entity #'nil)
(p/def ^:deprecated attribute nil)
(p/def ^:deprecated value #'nil)
(p/def ^:deprecated options-attribute #'nil)


(p/defn tx [v' props]
  (if-let [txfn (::tx props)]
    (p/$ txfn v')
    (when v'
      (let [[>e a _] (first context)]
        [[:db/add ~>e a v']]))))

(p/def ^:deprecated refs {}) ;; reference points in HFQL expr
(p/def ^:deprecated columns [])
(p/def ^:deprecated inputs [])

(defn quoted? [form]
  (and (seq? form)
       (= 'quote (first form))))

(p/defn join-all [v]
  (cond
    (quoted? v) v
    (map? v)    (into {} (p/for [[k v] v] [k ~v]))
    (list? v)   (p/for [v v] ~v)
    (coll? v)   (into (empty v) (p/for [v v] ~v))
    :else       v))

;; https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad.html#v:sequence
(p/defn sequenceM [>v _props] (p/$ join-all ~>v))

(p/defn render [>v props]
  (if-let [renderer (::render props)]
    (p/$ renderer >v props)
    (if-let [link (::link props)]
      (str "<a href=\"" (pr-str ~(second link)) "\">" (p/$ join-all ~>v) "</a>")
      (p/$ join-all ~>v))))

(p/defn data [>v] (binding [render sequenceM] (p/$ render >v nil)))


(tests
 (p/run (! (p/$ join-all #'[#'1 #'2 #'3])))
 % := [1 2 3])

(tests
 (p/run (! (p/$ join-all #'(list #'1 #'2 #'3))))
 % := '(1 2 3))

(tests
 (p/run (! (p/$ join-all #'{:a #'1, :b #'2, :c #'3})))
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
 (d/q '[:find [?e ...] :where [_ :dustingetz/gender ?g] [?g :db/ident ?e]] *$*)
 := [:dustingetz/male :dustingetz/female]
 (m/? (m/reduce conj (q '[:find [?e ...] :where [_ :dustingetz/gender ?g] [?g :db/ident ?e]])))
 := [[:dustingetz/male :dustingetz/female]])

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
 (nav! *$* 14 :dustingetz/email)
 := "alice@example.com"

 (m/? (m/reduce conj (nav *$* 14 :dustingetz/email)))
 := ["alice@example.com"])

(def rules
  '[[(hyperfiddle.api/needle-match ?v ?needle)
     [(str ?v) ?v']
     [(str ?needle) ?needle']
     #_[(.toLowerCase ?v')]
     #_[(.toLowerCase ?needle')]
     #_[(clojure.string/includes? ?v' ?needle')]
     [(clojure.string/includes? ?v' ?needle')]]])

(def exports (vars rules q nav *$* quoted? ->DB))
