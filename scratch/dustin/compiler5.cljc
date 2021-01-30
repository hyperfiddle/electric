(ns dustin.compiler5
  (:require [missionary.core :as m]))

(defn <- "In a reactive context, reacts to given flow." [flow]
  (throw (ex-info "Use only in quoted code." {})))

(defn >> "In a reactive context, splices (patch ?) parallel branches into a vector" [form])

(defn << "In a reactive context, splits (diff ?) given sequence into parallel branches,
   stabilized by given key function." [keyfn coll])

(comment
  '(mlet [[fiddle needle] first >route]
     (fmap vector
       (render-table (fiddle needle))
       (mlet [open identity >popover-open]
         (case open
           true (render-table (fiddle "tempid"))
           false (pure ::nothing)))))

  '(bind >route
     (fn [[fiddle needle]]
       (fmap vector
         (render-table (fiddle needle))
         (bind >popover-open
           (fn [open]
             (case open
               true (render-table (fiddle "tempid"))
               false (pure ::nothing)))))))

  `(let [[fiddle# needle#] (<- >route)]
     [(<- (render-table (fiddle# needle#)))
      (case (<- >popover-open)
        true (<- (render-table (fiddle# "tempid")))
        false ::nothing)])

  '(let [[fiddle needle] (<- >route)]
     (vector
       (<- (render-table (fiddle needle)))
       (<- (<- (case (<- >popover-open)
                 true (render-table (fiddle "tempid"))
                 false ::nothing)))))
  )

(comment
  ; what does the trace look like in this example?

  [{[0]                             [#{`submission-master} #{}]
    [0 `submission-master 2]        [#{} #{true}]
    [0 `submission-master 2 true 3] {:db/id "tempid"}}
   {[0 `submission-master 2]       [#{true} #{false}]
    [0 `submission-master 2 false] ::nothing}])


;; is mlet just sugar or first-class ? leo : irrelevant, geoffrey : depends



(comment
  (def !a (atom 0))
  (def !b (atom 0))

  `(let [a# (<- (m/watch !a))
         b# (<- (m/watch !b))]
     (+ (inc a#) (dec b#)))

  ;; naive continuation style, parallelism is lost
  '(bind (m/watch !a)
     (fn [a]
       (bind (m/watch !b)
         (fn [b]
           (pure (+ (inc a) (dec b)))))))

  ;; applicative style, parallelism OK
  '(fmap (fn [a b]
             (+ (inc a) (dec b)))
     (m/watch !a) (m/watch !b))

  ;; best of both worlds
  '((lift2 (fn [a b] (pure (+ (inc a) (dec b)))))
    (m/watch !a) (m/watch !b))

  ;; a closure react to its free variables. function is not rebuilt when b changes
  `(let [a# (<- (m/watch !a))
         b# (<- (m/watch !b))]
     ((fn [x#] (+ (inc a#) x#)) (dec b#)))

  ;; this is equivalent, standard function application
  `(let [a# (<- (m/watch !a))
         b# (<- (m/watch !b))]
     ((partial + (inc a#)) (dec b#)))

  ;; this is wrong. lambdas can be passed outside of dataflow language and called any time.
  `(let [b# (<- (m/watch !b))]
     (map (fn [x#] (+ (inc (<- (m/watch !a))) x#)) [(dec b#)]))

  ;; conditionals. the second vector element depends on >route, but only if popover is true.
  `(let [[fiddle# needle#] (<- >route)]
     [(<- (render-table (fiddle# needle#)))
      (case (<- >popover-open)
        true (<- (render-table (fiddle# "tempid")))
        false ::nothing)])

  ;; reactive for
  `(let [needle# (<- (m/watch !needle))
         subs# (submissions needle#)]
     (into [:table] (>> [:tr (<< :db/id subs#)])))

  [:table
   [:tr 1]
   [:tr 2]
   [:tr 3]]

  ;; no reactivity
  `(let [subs# [{:db/id 1} {:db/id 2} {:db/id 3}]]
     (into [:table] (>> [:tr (<< :db/id subs#)])))
  
  )