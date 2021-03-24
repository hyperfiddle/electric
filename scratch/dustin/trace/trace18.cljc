(ns dustin.trace18
  (:require [minitest :refer [tests]]
            [missionary.core :as m]))

;; TODO [1/5]
;; - [X] bind
;; - [ ] pure
;; - [ ] sequence
;; - [ ] unsequence
;; - [ ] expand AST to DAG definition
;; #+begin_src clojure
;; (expand
;;  (let [>a (input)
;;        >b (vector ~>a)]))
;; :=
;; (let [>a (m/signal! (m/watch (get @inputs '>a)))
;;       >b (m/signal! (m/latest vector >a))])
;; #+end_src


;; For this example, we assume the AST is complete We don’t see inside of
;; clojure functions. We only see the dataflow language. We can assign a unique
;; ID at any depth, including nested binds because we got them in the AST.
(def ast2 '(let [>needle (input)
                 >xs     (fmap range >needle)
                 >>xs    (unsequence identity >xs)
                 >rows   (bind >>xs (fn [>xs]
                                      (sequence >xs)))]))

;; (defn duplicate
;;   "duplicate :: (Comonad w) => w a -> w (w a)"
;;   [>wa]
;;   (fmap (constantly >wa) >wa))

(defn extend-seq [>as]
  (->> >as
       (fmap (fn [as] ;; [1 2 3]
               (map (fn [a] (fmap (constantly a) >as))
                    as)))))

(defn init [inputs _!replayers tracef]
  ;; >new-dynamic-nodes will collect new nodes {id val} and pass them to >effects
  ;; We must overload pure to put on to >new-dynamic-nodes.
  (let [>new-dynamic-nodes (…) ;; TODO
        >needle            (m/signal! (m/watch (get inputs '>needle)))
        >rows              (m/signal! (m/relieve {} (m/ap (m/?! (let [needle (m/?! >needle)]
                                                                  ;; TODO
                                                                  (unsequence (pure ["Match 1"
                                                                                     (str "Match " needle)
                                                                                     "Match 3"])))))))
        >effects           (m/stream! (m/relieve merge (m/ap
                                                        (amb=
                                                         {'>needle (m/?? >needle)}
                                                         {'>rows (m/?? >rows)}
                                                         ;; TODO
                                                         (m/?? >new-dynamic-nodes)))))]
    (m/stream! (m/ap (tracef (m/?? >effects))))))


(tests
 (def !needle (atom ""))
 (def r (reactor! {'>needle !needle} init))
 (def !trace (log! r))

 (directive! r '[>needle "Cat"])

 ;; UUID v4. Are UUIDs correct IDs for dynamic nodes What if we compute the same
 ;; bind node on both server and client? -> IDs don’t line up. It’s problematic
 ;; if we want to realign these two graphs. The server said "Here is what
 ;; happened under this bind", but the client did compute it itself so IDs don’t
 ;; line up.

 @!trace := '[{>needle "Cat"
               ;; [(static-id, react-key)+ ]
               [0 0]   "Match 1"
               [0 1]   "Match Cat"
               [0 2]   "Match 3"
               >rows   [[0 0] [0 1] [0 2]] ; bind always contains refs.
               ;; Should we only trace diffs?
               }]

 ;; For now we consider that bind nodes computes only on one peer at a time.
 ;; Therefore the produced dynamic graph is not reconciled, so UUIDs are good
 ;; enough for the job. This will bite us if we need any kind of reconciliation,
 ;; and we might leverage consistent hashing (path).

 ;; TODO get this right
 (->> (get @r '>rows)
      (capI)
      (mapv joinI))
 := ["Match 1"
     "Match Cat"
     "Match 3"]

 )



'[(defn foo [input] (extend-seq (partial range 0 2) input))
  (let [>needle (input)
        >rows   (bind >needle foo)])]
