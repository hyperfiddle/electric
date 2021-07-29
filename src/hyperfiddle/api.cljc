(ns hyperfiddle.api
  (:require [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as photon :refer [vars]]))

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

;; HTML <a> used in HFQL
(photon/def a nil)

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

(def exports (vars rules ->Link))