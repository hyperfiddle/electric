(ns hyperfiddle.rcf.utils
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn pprint-str [x]
  (with-out-str (pprint/pprint x)))

(defn pprint [x]
  (print (str/trimr (pprint-str x))))

(defn testing-vars-str [m]
  (let [{:keys [file line]} m]
    (str file ":" line )))
