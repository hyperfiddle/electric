(ns contrib.ednish2 ; URL safe EDN encoding with some visual niceties. Not really ednish, should be renamed
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [hyperfiddle.rcf :as rcf]))

(defn replace-all [str kvs]
  (reduce (fn [r [k v]] (str/replace r k v)) str kvs))

(defn encode-percent [str]
  (replace-all
    #?(:clj (java.net.URLEncoder/encode str)
       :cljs (js/encodeURIComponent str))
    {"!" "%21"
     "+" "%20"
     "'" "%27"}))

(defn decode-percent [str]
  #?(:clj (java.net.URLDecoder/decode str)
     :cljs (js/decodeURIComponent str)))

(def safe-chars
  [#_["%22" "'"] ; would required user-value escaping of '
   #_["%2F" "!"] ; nicer keywords, but prevent ! in user inputs
   ["%2C" ","]
   ["%3A" ":"]
   ["%28" "("]
   ["%29" ")"]
   ["%5B" "("] ; square brackets collapse to parens on purpose
   ["%5D" ")"]
   ;; ["%23" "~"] ; already nicely rendered
   ["%24" "$"]
   ["%26" "&"]
   ["%3B" ";"]
   ["%3D" "="]
   #_["%25" "%"] ; would break url encoding
   ])

(defn- invert [kvs] (map (fn [[k v]] [v k]) kvs))

(defn -encode-safe-chars [str] (reduce (fn [r [k v]] (str/replace r k v)) str safe-chars))
(defn -decode-safe-chars [str] (reduce (fn [r [k v]]
                                        (if-not (= "%" k)
                                          (str/replace r k v)
                                          r))
                                str (invert safe-chars)))

(defn encode [value]
  (-encode-safe-chars (encode-percent (pr-str value))))

(hyperfiddle.rcf/tests
  (encode :hyperfiddle.blog/post) := ":hyperfiddle.blog%2Fpost"
  (encode :a!b) := ":a%21b"
  (encode "kobe") := "%22kobe%22"
  (encode #{"events" "news"}) := "%23%7B%22news%22%20%22events%22%7D"
  (encode #uuid "07655f77-608d-472b-bc5e-86fcecc40b00")
  := "%23uuid%20%2207655f77-608d-472b-bc5e-86fcecc40b00%22"
  )

(defn decode [ednish-str]
  (edn/read-string (decode-percent (-decode-safe-chars ednish-str))))

(rcf/tests
  (decode (encode :hyperfiddle.blog/post)) := :hyperfiddle.blog/post
  (decode (encode :a!b)) := :a!b
  (decode (encode "kobe")) := "kobe"
  (decode (encode #{"events" "news"})) := #{"events" "news"}
  (decode (encode #uuid "07655f77-608d-472b-bc5e-86fcecc40b00"))
  := #uuid "07655f77-608d-472b-bc5e-86fcecc40b00"
  )

(rcf/tests
  "url encoding"
  (encode "|") := "%22%7C%22"
  (decode (encode "|")) := "|"

  (encode "!$&'[]()*+,;=|")
   := "%22%21$&%27()()*%2B,;=%7C%22"
  (decode (encode "!$&'[]()*+,;=|"))
  )

