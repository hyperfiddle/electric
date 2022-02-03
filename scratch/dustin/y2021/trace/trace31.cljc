(ns dustin.trace31)

(def !a (atom nil))
(def !b (atom nil))
(def !c (atom #{}))

(def extended-fmap-ast1
  '(let [>a (missionary.core/watch dustin.trace31/!a)
         >b (missionary.core/watch dustin.trace31/!b)
         >c (fmap clojure.core/+ >a >b)]))

(def extended-fmap-ast2
  `(let [>a# (m/watch !a)
         >b# (m/watch !b)
         >c# (fmap + >a# >b#)]))

(def extended-fmap-ast3
  [:let ['>a `(m/watch !a)
         '>b `(m/watch !b)
         '>c [:fmap `+ '>a '>b]]])

;; let could be a map, toposort can be done by compiler
(def extended-fmap-ast4
  [:dag {'>c [:fmap `+ '>a '>b]
         '>d '>a
         '>a `(m/watch !a)
         '>b `(m/watch !b)}])

(def extended-fmap-ast5
  [:fmap
   `client-only
   [:fmap `server-only [:user `(m/watch !a)] [:user `(m/watch !b)]]])

(mlet [a (m/watch !a)
       b (expensive a)
       c (another-computation b)])
(m/signal! (m/latest (comp another-computation expensive) (m/watch !a)))

(let {a1 (m/watch !a1)
      a2 (m/watch !a2)
      b  (m/latest + a1 a2)
      c  (m/latest * a1 a2)
      d  (m/latest vector b c)})

(let [a1 (m/signal! (m/watch !a1))
      a2 (m/signal! (m/watch !a2))
      d (m/signal! (m/latest vector (m/latest + a1 a2) (m/latest * a1 a2)))])
[:fmap `vector
 [:fmap `+ [:user `(m/watch !a1)] [:user `(m/watch !a2)]]
 [:fmap `* [:user `(m/watch !a1)] [:user `(m/watch !a2)]]]

(def extended-fmap-ast6
  [:for ['>a identity `[0 1 2]
         '>b identity (range '>a)
         '>c identity (range '>b)]
   (client-only (server-only '>b) (client-only2 3))])

(def extended-fmap-ast7
  (magic-for
    (let [>b (expand identity (range (expand identity [0 1 2])))
          >c (expand identity (range >b))]
      (client-only (server-only >b) (client-only2 3)))))

[0 []
 1 [0 []]
 2 [0 [] 1 []]]


