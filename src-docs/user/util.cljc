(ns user.util
  (:require clojure.string
            [hyperfiddle.rcf :refer [tests]]
            [clojure.pprint :as pprint]))

(defn includes-str? "used repeatedly in tutorials" [v needle]
  (clojure.string/includes? (clojure.string/lower-case (str v))
                            (clojure.string/lower-case (str needle))))

(tests
  (includes-str? "alice" "e") := true
  (includes-str? "alice" "f") := false
  (includes-str? "alice" "") := true
  (includes-str? "alice" nil) := true
  (includes-str? nil nil) := true
  (includes-str? nil "") := true
  (includes-str? "" nil) := true
  )

(defn pprint-str [x]
  (with-out-str
    (pprint/with-pprint-dispatch pprint/code-dispatch
      (pprint/pprint x))))
