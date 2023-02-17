(ns geoffrey.y2023.destructure-bug
  #?(:cljs (:require-macros [geoffrey.y2023.destructure-bug]))
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric.impl.compiler :as c]
            [hyperfiddle.electric.impl.runtime :as r]))

(def !x (atom 0))

(e/defn App []
  (def foo (e/watch !x))
  (e/server (def foo (e/watch !x)) nil))


(comment
  (swap! !x inc)

  (c/analyze {} '(App.))

  (binding [cljs.env/*compiler* (cljs.env/default-compiler-env)]
    (c/analyze (c/normalize-env (merge (cljs.analyzer/empty-env) {:ns {:name 'geoffrey.y2023.destructure-bug}
                                                                  ::c/peers-config {::c/local :cljs ::c/remote :clj}}))
      '(geoffrey.y2023.destructure-bug/App.)))
  )

(comment
  (set! *print-namespace-maps* false)
  (def server (second (c/analyze {} '(App.)))) 
  (def client (first (c/analyze {} '(App.)))) 
  server 
  (first (c/analyze {} '(App.))) 

  (r/emit nil server)
  (r/emit nil client)


  (binding [cljs.env/*compiler* (cljs.env/default-compiler-env)]
    (c/analyze (c/normalize-env (merge (cljs.analyzer/empty-env) {:ns {:name 'geoffrey.y2023.destructure-bug}
                                                                  ::c/peers-config {::c/local :cljs ::c/remote :clj}}))
      '(e/server (def foo 1))))

  )

(comment
  (binding [*ns* (find-ns 'user.electric.electric-diffing)]
    (eval '(def user.electric.electric-diffing/bar 1))))


(comment ; SERVER

  ;; Error
  ;; ExceptionInfo: failed compiling constant: class clojure.lang.PersistentArrayMap; java.lang.Class is not a valid ClojureScript constant.

  {:hyperfiddle.electric.impl.ir/op :hyperfiddle.electric.impl.ir/do,
   :hyperfiddle.electric.impl.ir/deps
   [{:hyperfiddle.electric.impl.ir/op :hyperfiddle.electric.impl.ir/target,
     :hyperfiddle.electric.impl.ir/deps
     [{:hyperfiddle.electric.impl.ir/op :hyperfiddle.electric.impl.ir/target,
       :hyperfiddle.electric.impl.ir/deps
       [{:hyperfiddle.electric.impl.ir/op :hyperfiddle.electric.impl.ir/output,
         :hyperfiddle.electric.impl.ir/init
         {:hyperfiddle.electric.impl.ir/op :hyperfiddle.electric.impl.ir/apply,
          :hyperfiddle.electric.impl.ir/fn
          {:hyperfiddle.electric.impl.ir/op :hyperfiddle.electric.impl.ir/eval,
           :hyperfiddle.electric.impl.ir/form
           (hyperfiddle.electric.impl.compiler/fn-call
             (fn*
               ([p__58651]
                (let*
                    [map__58653
                     p__58651
                     map__58653
                     (if
                         (clojure.core/seq? map__58653)
                       (if
                           (clojure.core/next map__58653)
                         (.
                           clojure.lang.PersistentArrayMap
                           (createAsIfByAssoc (clojure.core/to-array map__58653)))
                         (if
                             (clojure.core/seq map__58653)
                           (clojure.core/first map__58653)
                           (. clojure.lang.PersistentArrayMap -EMPTY)))
                       map__58653)]
                  nil)))
             nil),
           :hyperfiddle.electric.impl.ir/ns user,
           :hyperfiddle.electric.debug/action :fn-call,
           :hyperfiddle.electric.debug/params […]},
          :hyperfiddle.electric.impl.ir/args nil},
         :hyperfiddle.electric.debug/meta
         {:file "geoffrey/y2023/destructure_bug.cljc",
          :line 9,
          :column 3,
          :hyperfiddle.electric.debug/type :transfer,
          :hyperfiddle.electric.debug/name :hyperfiddle.electric/server}}]}
      {:hyperfiddle.electric.impl.ir/op :hyperfiddle.electric.impl.ir/source}]}],
   :hyperfiddle.electric.impl.ir/inst
   {:hyperfiddle.electric.impl.ir/op :hyperfiddle.electric.impl.ir/do,
    :hyperfiddle.electric.impl.ir/deps
    [{:hyperfiddle.electric.impl.ir/op :hyperfiddle.electric.impl.ir/source}],
    :hyperfiddle.electric.impl.ir/inst
    {:hyperfiddle.electric.impl.ir/op :hyperfiddle.electric.impl.ir/nop}}}
  )

;; `(~clojure.lang.PersistentArrayMap)
;; (quote clojure.lang.PersistentArrayMap)

;; Originates from `cljs.compiler/emit-constant`. The cljs compiler should not see `PersistentArrayMap`

(comment
  ;; What is being emitted?


  (e/defn App [] (e/server (fn [{:keys []}])))

  ```
  ;; Form:
  (fn [{:keys []}])

  ;; Macroexpanded
  (fn* ([p__58749] (let [{:keys []} p__58749] ) ))

  ;; Macroexpanded further
  (fn* ([p__58749]
        (let* [map__58750 p__58749
               map__58750 (if (seq? map__58750)
                            (if (next ^clojure.lang.ISeq map__58750)
                              (clojure.lang.PersistentArrayMap/createAsIfByAssoc
                                (to-array ^clojure.lang.ISeq map__58750))
                              (if (seq ^clojure.lang.ISeq map__58750)
                                (first ^clojure.lang.ISeq map__58750)
                                clojure.lang.PersistentArrayMap/EMPTY))
                            map__58750)])))

  ```

  ;; cljs compiler complains clojure.lang.PersistentArrayMap is a java.lang.Class and not a valid ClojureScript constant.
  ;; Electric compiler analyzes this code and return a class object for clojure.lang.PersistentArrayMap instead of a symbol.


  (second (hyperfiddle.electric.impl.compiler/analyze {} '(hyperfiddle.electric/server (fn [{:keys []}])))))
