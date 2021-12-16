(ns hyperfiddle.api
  (:require [datascript.core :as d]
            [hyperfiddle.rcf :refer [tests ! %]]
            [hfdl.lang :as p :refer [vars]]
            [missionary.core :as m]
            #?(:clj [hyperfiddle.dev.logger :as log]))
  #?(:cljs (:require [hyperfiddle.dev.logger :as log]))
  #?(:cljs (:require-macros [hyperfiddle.dev.logger :refer [debug]]
                            [hyperfiddle.api :refer [db entity attribute value sequenceM render join-all]])))

(def ^:dynamic *$*)                                         ; available in cljs for HFQL datascript tests
(def ^:dynamic *route*)                                     ; cljs

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

(p/def db nil)
(p/def entity #'nil)
(p/def attribute #'nil)
(p/def value #'nil)
(p/def options-attribute #'nil)

(p/def columns [])
(p/def inputs [])

(p/defn join-all [v]
  (cond
    (map? v)  (into {} (p/for [[k v] v] [k ~v]))
    (list? v) (p/for [v v] ~v)
    (coll? v) (into (empty v) (p/for [v v] ~v))
    :else     v))

;; https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Monad.html#v:sequence
(p/def sequenceM #'(let [v ~value]
                     (debug "sequenceM" v)
                     (p/$ join-all v)))

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

(p/def render sequenceM)

(p/def link #'~value)

(p/def props {})
(p/def args  {})

(p/defn flatten-1 [>v]
  (let [render' render]
    (binding [render #'~value]
      (let [v ~>v]
        (binding [render render']
          (p/$ join-all v))))))

; todo rename wrap, it's sideeffect-fn to fn-returning-flow
(defn wrap [f] (fn [& args] #?(:clj (m/ap (m/? (m/via m/blk (apply f args))))
                               :cljs (m/ap (apply f args))))) ; m/via not supported in cljs

(def q (wrap (fn [query & args]
               (log/debug :q query args)
               (doto (apply d/q query *$* args) log/debug))))

(defn nav!
  ([_ ref] ref)
  ([db ref kf] (kf (d/entity db ref)))
  ([db ref kf & kfs] (reduce (partial nav! db) (nav! db ref kf) kfs)))

(def nav (wrap (fn [ref & kfs]
                 (log/debug :nav ref kfs)
                 (doto (apply nav! *$* ref kfs) log/debug))))

(tests
  (nav! *$* 9 :dustingetz/email)
  := "alice@example.com"

  (m/? (m/reduce conj (nav 9 :dustingetz/email)))
  := ["alice@example.com"])

(def exports (vars rules ->Link info q nav))
