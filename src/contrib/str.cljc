(ns contrib.str
  (:require clojure.pprint
            clojure.string
            [hyperfiddle.rcf :refer [tests]]))

(defn pprint-str [x]
  (with-out-str
    (clojure.pprint/with-pprint-dispatch
      clojure.pprint/code-dispatch
      (clojure.pprint/pprint x))))

(defn ^:deprecated includes-str? [v needle]
  ; perf - https://clojurians.slack.com/archives/C03RZMDSH/p1666290300539289
  (clojure.string/includes? (clojure.string/lower-case (str v))
                            (clojure.string/lower-case (str needle))))

(tests
  (includes-str? "alice" "e") := true
  (includes-str? "alice" "f") := false
  (includes-str? "alice" "") := true
  (includes-str? "alice" nil) := true
  (includes-str? nil nil) := true
  (includes-str? nil "") := true
  (includes-str? "" nil) := true)
