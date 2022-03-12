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
  #?(:cljs (:require-macros [hyperfiddle.dev.logger :refer [debug]]
                            [hyperfiddle.api :refer [route db entity attribute value context refs props sequenceM render join-all data tx]])))

(def ^:dynamic *$*)                                         ; available in cljs for HFQL datascript tests

(defmacro hfql [& body])
(defmacro page [& body])
(defmacro app [& body])

(defn needle-match [v needle]
  (clojure.string/includes?
    (.toLowerCase (or (str v) ""))
    (.toLowerCase (or (str needle) ""))))

(tests
  (needle-match "alice" "a") := true
  (needle-match "alice" "A") := true
  (needle-match "alice" "b") := false)

(defn needle-match' [v needle]
  (boolean (re-find #?(:clj  (re-pattern (str "(?i)" needle))
                       :cljs (js/RegExp. needle "i")) v)))

(tests
  (needle-match' "alice" "a") := true
  (needle-match' "alice" "A") := true
  (needle-match' "alice" "b") := false)

(def rules
  '[[(hyperfiddle.api/needle-match ?v ?needle)
     [(str ?v) ?v']
     [(str ?needle) ?needle']
     #_[(.toLowerCase ?v')]
     #_[(.toLowerCase ?needle')]
     #_[(clojure.string/includes? ?v' ?needle')]
     [(clojure.string/includes? ?v' ?needle')]]])

;;;;;;;;;;;;;;;;;;;;
;; Semantic Types ;;
;;;;;;;;;;;;;;;;;;;;

(deftype Link [href value]
  Object
  (toString [this]
    (str "#hyperfiddle.api.Link " {:href href, :value value}))
  (equals [this other]
    (and (= href (.href other))
         (= value (.value other)))))

(deftype Input [id value onChange]
  Object
  (toString [this]
    (str "#hyperfiddle.api.Input" {:id    id
                                   :value value}))
  (equals [this other]
    (= (.id this) (.id other))))

#?(:clj (defmethod print-method Link [^Link v w]
          (.write w (.toString v))))

#?(:clj (defmethod print-method Input [^Input v w]
          (.write w (.toString v))))

#?(:cljs (cljs.reader/register-tag-parser! 'hyperfiddle.api.Link (fn [{:keys [href value]}] (Link. href value))))

#?(:cljs (cljs.reader/register-tag-parser! 'hyperfiddle.api.Input (fn [{:keys [id value]}] (Input. id value nil))))

#?(:cljs (extend-protocol IPrintWithWriter
           Link
           (-pr-writer [this writer _]
             (write-all writer "#hyperfiddle.api.Link " (pr-str {:href  (.-href this)
                                                                 :value (.-value this)})))
           Input
           (-pr-writer [this writer _]
             (write-all writer "#hyperfiddle.api.Input " (pr-str {:id    (.-id this)
                                                                  :value (.-value this)})))))

(def info (atom "hyperfiddle"))

(p/def route (atom nil))
(p/def db nil)
(p/def entity #'nil)
(p/def attribute nil)
(p/def value #'nil)
(p/def options-attribute #'nil)
(p/def context nil)

(p/defn tx [v' props]
  (if-let [txfn (::tx props)]
    (p/$ txfn v')
    (let [[>e a _] (first context)]
      [[:db/add ~>e a v']])))

(p/def refs {}) ;; reference points in HFQL expr
(p/def columns [])
(p/def inputs [])

(defn quoted? [form]
  (and (seq? form)
       (= 'quote (first form))))

(p/defn join-all [v]
  ;; (prn "join-all" v)
  (cond
    (quoted? v) v
    (map? v)    (into {} (p/for [[k v] v] [k ~v]))
    (list? v)   (p/for [v v] ~v)
    (coll? v)   (into (empty v) (p/for [v v] (do (prn v) ~v)))
    :else       v))

(p/defn render [>v props]
  (if-let [renderer (::render props)]
    (p/$ renderer >v props)
    (if-let [link (::link props)]
      (str "<a href=\"" (pr-str ~(second link)) "\">" (p/$ join-all ~>v) "</a>")
      (p/$ join-all ~>v))))

;; https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad.html#v:sequence
(p/defn sequenceM [>v _props] (p/$ join-all ~>v))

(p/defn data [>v] (binding [render sequenceM] (p/$ render >v nil)))

;; (p/run (binding [value #'{:a #':b}]
;;          (prn ~sequenceM)))

(tests
  (p/run (! (binding [value #'[#'1 #'2 #'3]]
              ~sequenceM)))
  % := [1 2 3])

(tests
  (p/run (! (binding [value #'(list #'1 #'2 #'3)]
              ~sequenceM)))
  % := '(1 2 3))

(tests
  (p/run (! (binding [value #'{:a #'1, :b #'2, :c #'3}]
              ~sequenceM)))
  % := '{:a 1, :b 2, :c 3})

(p/def link #'~value)

(p/def props {})
(p/def args  {})

; todo rename wrap, it's sideeffect-fn to fn-returning-flow
(defn wrap [f] (fn [& args] #?(:clj (m/ap #_(m/? (m/via m/blk (apply f args))) ;; TODO restore and handle continuous flow initial state
                                          (apply f args))
                               :cljs (m/ap (apply f args))))) ; m/via not supported in cljs

(def q (wrap (fn [query & args]
               (log/debug :q query args)
               (doto (apply d/q query *$* args) log/debug))))

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

(def nav (wrap (fn [e & as]
                 (log/debug :nav e as)
                 (doto (apply nav! *$* e as) log/debug))))

(tests
  (nav! *$* 14 :dustingetz/email)
  := "alice@example.com"

  (m/? (m/reduce conj (nav 14 :dustingetz/email)))
  := ["alice@example.com"])

(def exports (vars rules ->Link info q nav *$* quoted?))
