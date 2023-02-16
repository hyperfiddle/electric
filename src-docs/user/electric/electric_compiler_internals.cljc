(ns user.electric.electric-compiler-internals
  (:require [hyperfiddle.electric :as e]
            #?(:clj [hyperfiddle.electric.impl.compiler :refer [analyze]])
            #?(:clj [hyperfiddle.electric.impl.runtime :as r :refer [emit]])
            [hyperfiddle.electric.impl.ir :as ir]
            [hyperfiddle.electric.debug :as debug]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m]))

(tests
  "Photon compiler"
  (def !x (atom 0))

  ; client only first example
  (def env {})
  (analyze env '(let [x (new (m/watch !x))
                      a (e/server (inc x))]
                  (rcf/tap (+ a (inc x)))))
  ; we analyze the DAG for connections - by analyzing dependencies in the AST


  ; Intermediate Language for DAGs.
  ; first element is the client DAG,
  ; second element is server DAG

  :=
  '[{::ir/op   ::ir/pub,
     ::ir/init {::ir/op   ::ir/pub,
                ::ir/init {::ir/op   ::ir/apply,
                           ::ir/fn   {::ir/op ::ir/global, ::ir/name :missionary.core/watch, ::debug/meta {}},
                           ::ir/args ({::ir/op      ::ir/global,
                                       ::ir/name
                                       :user.electric.electric-compiler-internals/!x,
                                       ::debug/meta {}})},
                ::ir/inst {::ir/op   ::ir/apply,
                           ::ir/fn   {::ir/op    ::ir/literal,
                                      ::ir/value {}},
                           ::ir/args ({::ir/op    ::ir/sub,
                                       ::ir/index 1}
                                      {::ir/op   ::ir/pub,
                                       ::ir/init {::ir/op    ::ir/literal,
                                                  ::ir/value 0},
                                       ::ir/inst {::ir/op ::ir/apply,
                                                  ::ir/fn
                                                  {::ir/op    ::ir/literal,
                                                   ::ir/value {}},
                                                  ::ir/args
                                                  ({::ir/op ::ir/sub, ::ir/index 1}
                                                   {::ir/op    ::ir/bind,
                                                    ::ir/slot  0,
                                                    ::ir/index 1,
                                                    ::ir/inst  {::ir/op ::ir/variable,
                                                                ::ir/init
                                                                {::ir/op    ::ir/sub,
                                                                 ::ir/index 2}}})}})}},
     ::ir/inst {::ir/op   ::ir/apply,
                ::ir/fn   {::ir/op ::ir/literal, ::ir/value {}},
                ::ir/args ({::ir/op ::ir/sub, ::ir/index 1}
                           {::ir/op   ::ir/pub,
                            ::ir/init {::ir/op   ::ir/input,
                                       ::ir/deps [{::ir/op   ::ir/output,
                                                   ::ir/init {::ir/op       ::ir/sub,
                                                              ::ir/index    1,
                                                              ::debug/name  x,
                                                              ::debug/scope :lexical,
                                                              ::debug/meta  nil}}]},
                            ::ir/inst {::ir/op   ::ir/apply,
                                       ::ir/fn   {::ir/op ::ir/literal, ::ir/value {}},
                                       ::ir/args ({::ir/op ::ir/sub, ::ir/index 1}
                                                  {::ir/op   ::ir/apply,
                                                   ::ir/fn   {::ir/op ::ir/global, ::ir/name :hyperfiddle.rcf/tap, ::debug/meta {}},
                                                   ::ir/args ({::ir/op   ::ir/apply,
                                                               ::ir/fn   {::ir/op ::ir/global, ::ir/name :clojure.core/+, ::debug/meta {}},
                                                               ::ir/args ({::ir/op       ::ir/sub,
                                                                           ::ir/index    1,
                                                                           ::debug/name  a,
                                                                           ::debug/scope :lexical,
                                                                           ::debug/meta  nil}
                                                                          {::ir/op   ::ir/apply,
                                                                           ::ir/fn   {::ir/op ::ir/global, ::ir/name :clojure.core/inc, ::debug/meta {}},
                                                                           ::ir/args ({::ir/op
                                                                                       ::ir/sub,
                                                                                       ::ir/index    2,
                                                                                       ::debug/name  x,
                                                                                       ::debug/scope :lexical,
                                                                                       ::debug/meta  nil})})})})}})}}
    {::ir/op   ::ir/do,
     ::ir/deps [{::ir/op ::ir/source}
                {::ir/op      ::ir/output,
                 ::ir/init    {::ir/op   ::ir/apply,
                               ::ir/fn   {::ir/op ::ir/global, ::ir/name :clojure.core/inc, ::debug/meta {}},
                               ::ir/args ({::ir/op       ::ir/input,
                                           ::ir/deps     [],
                                           ::debug/name  x,
                                           ::debug/scope :dynamic,
                                           ::debug/meta  nil})},
                 ::debug/meta {:line        17,
                               :column      25,
                               ::debug/type :transfer,
                               ::debug/name :hyperfiddle.electric/server}}],
     ::ir/inst {::ir/op ::ir/nop}}]

  (def client (first *1))
  (def server (second *2))


  (tests
    "emitted reactive target program for client"
    (emit nil client)
    :=
    '(hyperfiddle.electric.impl.runtime/peer 1 [] 1 0 0 0 1 1
       (clojure.core/fn [-frame -vars]
         (clojure.core/let [-pub-0 (hyperfiddle.electric.impl.runtime/signal
                                     (clojure.core/let [-pub-0 (hyperfiddle.electric.impl.runtime/signal
                                                                 (hyperfiddle.electric.impl.runtime/latest-apply
                                                                   '{::ir/op      ::ir/global,
                                                                     ::debug/meta {},
                                                                     ::debug/type :apply,
                                                                     ::debug/name missionary.core/watch}
                                                                   (hyperfiddle.electric.impl.runtime/pure missionary.core/watch)
                                                                   (hyperfiddle.electric.impl.runtime/pure user.electric.electric-compiler-internals/!x)))]
                                       (hyperfiddle.electric.impl.runtime/latest-apply
                                         '{::debug/type :apply,
                                           ::debug/name {}}
                                         (hyperfiddle.electric.impl.runtime/pure '{})
                                         -pub-0
                                         (clojure.core/let [-pub-1 (hyperfiddle.electric.impl.runtime/signal
                                                                     (hyperfiddle.electric.impl.runtime/pure '0))]
                                           (hyperfiddle.electric.impl.runtime/latest-apply
                                             '{::debug/type :apply,
                                               ::debug/name {}}
                                             (hyperfiddle.electric.impl.runtime/pure '{})
                                             -pub-1
                                             (clojure.core/let [-prev (clojure.core/aget -vars 0)]
                                               (clojure.core/aset -vars (clojure.core/int 0) -pub-1)
                                               (clojure.core/let [-res (hyperfiddle.electric.impl.runtime/variable -frame -vars 0 0 -pub-0)]
                                                 (clojure.core/aset -vars (clojure.core/int 0) -prev)
                                                 -res)))))))]
           (hyperfiddle.electric.impl.runtime/latest-apply
             '{::debug/type :apply,
               ::debug/name {}}
             (hyperfiddle.electric.impl.runtime/pure '{})
             -pub-0
             (clojure.core/let [-pub-1 (hyperfiddle.electric.impl.runtime/signal
                                         (hyperfiddle.electric.impl.runtime/input-spawn -frame 0
                                           [(hyperfiddle.electric.impl.runtime/make-output 0
                                              (hyperfiddle.electric.impl.runtime/check-failure
                                                '{::ir/op ::ir/output}
                                                -pub-0))]))]
               (hyperfiddle.electric.impl.runtime/latest-apply
                 '{::debug/type :apply,
                   ::debug/name {}}
                 (hyperfiddle.electric.impl.runtime/pure '{})
                 -pub-1
                 (hyperfiddle.electric.impl.runtime/latest-apply
                   '{::ir/op      ::ir/global,
                     ::debug/meta {},
                     ::debug/type :apply,
                     ::debug/name hyperfiddle.rcf/tap}
                   (hyperfiddle.electric.impl.runtime/pure hyperfiddle.rcf/tap)
                   (hyperfiddle.electric.impl.runtime/latest-apply
                     '{::ir/op      ::ir/global,
                       ::debug/meta {},
                       ::debug/type :apply,
                       ::debug/name clojure.core/+}
                     (hyperfiddle.electric.impl.runtime/pure clojure.core/+)
                     -pub-1
                     (hyperfiddle.electric.impl.runtime/latest-apply
                       '{::ir/op      ::ir/global,
                         ::debug/meta {},
                         ::debug/type :apply,
                         ::debug/name clojure.core/inc}
                       (hyperfiddle.electric.impl.runtime/pure clojure.core/inc)
                       -pub-0))))))))))


  (tests
    "emitted server target program"
    (emit nil server)
    :=
    '(hyperfiddle.electric.impl.runtime/peer 0 [] 0 1 0 0 1 1
      (clojure.core/fn [-frame -vars]
        (do (hyperfiddle.electric.impl.runtime/make-input -frame
            [(hyperfiddle.electric.impl.runtime/source -frame -vars 0 0)
             (hyperfiddle.electric.impl.runtime/make-output 0
               (hyperfiddle.electric.impl.runtime/check-failure
                 '{::ir/op      ::ir/output,
                   ::debug/meta {:line        17,
                                 :column      25,
                                 ::debug/type :transfer,
                                 ::debug/name :hyperfiddle.electric/server}}
                 (hyperfiddle.electric.impl.runtime/latest-apply
                   '{::ir/op      ::ir/global,
                     ::debug/meta {},
                     ::debug/type :apply,
                     ::debug/name clojure.core/inc}
                   (hyperfiddle.electric.impl.runtime/pure clojure.core/inc)
                   (hyperfiddle.electric.impl.runtime/input-spawn -frame 0 []))))])
          nil)))))
