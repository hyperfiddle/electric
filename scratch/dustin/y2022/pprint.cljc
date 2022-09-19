(ns triage.pprint
  "for debugging cljs browser apps in production at the browser console without a repl"
  (:require
    clojure.pprint
    [hyperfiddle.rcf :refer [tests tap %]]))


(defn pprint-str [x]
  (with-out-str
    (binding [clojure.pprint/*print-right-margin* 20
              #_#_clojure.pprint/*print-miser-width* 1
              #_#_clojure.pprint/*print-pprint-dispatch* hyperfiddle.pprint/simple-dispatch]
      (clojure.pprint/with-pprint-dispatch
        clojure.pprint/code-dispatch
        (clojure.pprint/pprint x)))))

(comment
  (pprint-str (range 50))
  (pprint-str '{(user.gender-shirt-size/submissions "" .)
                [{:db/id 9}
                 {:db/id 10}
                 {:db/id 11}]})
  (pprint-str '{(user.gender-shirt-size/submissions "bob" .)
                [{:dustingetz/gender {:db/ident :dustingetz/male},
                  :dustingetz/email "bob@example.com",
                  :dustingetz/shirt-size {:db/ident :dustingetz/mens-large},
                  :db/id 10}]})
  )
