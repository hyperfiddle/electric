(ns contrib.css
  (:require clojure.string
            [hyperfiddle.rcf :refer [tests]]))

(def slugify-regexp #"[:/? .)(]")

(defn css-slugify
  "munge common types (qualified keywords and symbols, numbers) into sort of valid css. we make no attempt at a
  correct slug"
  [s]
  ; in CSS, identifiers (including element names, classes, and IDs in selectors) can contain only
  ; the characters [a-zA-Z0-9] and ISO 10646 characters U+00A0 and higher, plus the hyphen (-) and
  ; the underscore (_); they cannot start with a digit, two hyphens, or a hyphen followed by a digit.
  ; http://stackoverflow.com/a/449000/959627
  ; https://mathiasbynens.be/notes/css-escapes
  (-> (if (number? s)
        (str "n" s) ; "0" and "-0" is not legal css but "n0" is
        (str s)) ; coerce keywords etc
      (clojure.string/replace slugify-regexp "-")))

(tests
  (css-slugify `foo) := "contrib-css-foo"
  (css-slugify [1 ::two `three `4]) := "[1--contrib-css-two-contrib-css-three-4]"
  (css-slugify 1) := "n1"
  (css-slugify #uuid "00000000-0000-0000-0000-000000000000") := "00000000-0000-0000-0000-000000000000"
  (css-slugify nil) := ""

  "careful, the following are not valid css"
  (css-slugify "--") := "--"
  (css-slugify "1") := "1")

(defn css "&args will be flattened"
  [& args]
  (->> (flatten args)
       (remove nil?)
       (clojure.string/join " ")))
