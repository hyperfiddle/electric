(ns hyperfiddle.photon-xp
  (:require [missionary.core :as m])
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
  `(unquote (->> (var (do ~@body))
              (m/eduction (constantly nil) (dedupe))
              (m/reductions {} nil)
              (m/relieve {}))))

(defmacro deduping [x]
  `(unquote (->> (var ~x)
              (m/eduction (dedupe))
              (m/reductions {} nil)
              (m/relieve {}))))

(defn newest [>left >right] (m/ap (m/?< (m/amb= >left >right))))