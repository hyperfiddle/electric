(ns hyperfiddle.common.char
  (:require
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.test :refer [deftest is]]))


(defn char-upper [c]
  #?(:cljs (-> c string/upper-case)
     :clj  (-> c string/upper-case first)))

(defn char-lower [c]
  #?(:cljs (-> c string/lower-case)
     :clj  (-> c string/lower-case first)))

(def dec->hex {0 \0 1 \1 2 \2 3 \3 4 \4 5 \5 6 \6 7 \7 8 \8 9 \9 10 \A 11 \B 12 \C 13 \D 14 \E 15 \F})
(def hex->dec (merge (set/map-invert dec->hex)
                     (set/map-invert (->> dec->hex (reduce-kv (fn [m k v] (assoc m k (char-lower v))) {})))))

(defn char-code [c]
  {:pre [c]}
  ; cljs.user=> (map identity "abcd") => ("a" "b" "c" "d")
  ; cljs.user=> (map #(.charCodeAt %) "abcd") => (97 98 99 100)
  ;
  ; (map identity "abcd") => (\a \b \c \d)
  ; (map int "abcd") => (97 98 99 100)
  #?(:cljs (.charCodeAt c)
     :clj  (int c)))

(defn char->hex-str [c]
  {:pre [c]}
  ; Portable - the usual primitives are platform specific and weird interfaces
  ;{:pre [(< n 256) (> n 0)]}
  (->> (char-code c)
       ((juxt #(quot % 16) #(mod % 16)))
       (map dec->hex)
       string/join))

(defn hex-str->char [s]
  {:pre [(= 2 (count s))]}
  (let [[a b] (map hex->dec s)]
    (char (+ (* 16 (int a)) (int b)))))


(deftest char-code-1
  []
  (is (= (map char-code "abcd|")
         '(97 98 99 100 124))))

(deftest hex-1
  []
  (is (= (hex->dec \a) (hex->dec \A)))
  (is (= (char->hex-str \space) "20"))
  (is (= (char->hex-str \newline) "0A"))
  (is (not= (char->hex-str \newline) "0a"))
  (is (= ((comp hex-str->char char->hex-str) \space) \space))
  (is (= ((comp hex-str->char char->hex-str) \newline) \newline))
  (is (= ((comp hex-str->char char->hex-str) \a) \a)))
