(ns hyperfiddle.photon-xp
  (:require [missionary.core :as m]
            [hyperfiddle.photon :as p])
  #?(:cljs (:require-macros [hyperfiddle.photon-xp :refer [forget deduping]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         EXPERIMENTAL ZONE             ;;
;;                                       ;;
;; Everything below should be considered ;;
;; guilty until proven innocent          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn continuous
  ([>x] (continuous nil >x))
  ([init >x] (m/relieve {} (m/reductions {} init >x))))

(defmacro forget
  "Like `do` but returs `nil` once, then never return again."
  [& body]
  `(new (->> (p/fn [] ~@body)
          (m/eduction (constantly nil) (dedupe))
          (m/reductions {} nil)
          (m/relieve {}))))

(defmacro deduping [x]
  `(new (->> (p/fn [] ~x)
          (m/eduction (dedupe))
          (m/reductions {} nil)
          (m/relieve {}))))

(defn newest [>left >right] (m/ap (m/?< (m/amb= >left >right))))