(ns dustin.y2022.photon-transfer-bug
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests tap % with]]
            [hyperfiddle.photon-impl.compiler :refer [analyze]]
            [hyperfiddle.photon-impl.runtime :as r :refer [emit]]
            [missionary.core :as m]))

;m/signal! = a continuous flow that has an identity
;"continuous flow" = a recipe for an effect
;"signal"
;
;(m/signal!)
;(m/stream!)
;
;(m/bind!)
;(m/malloc!)
;(m/subject!)


(hyperfiddle.rcf/enable!)

(p/def db)
(p/def %1)

;(p/defn View [db] ~@(prn db))

(p/defn App []
  ~@(let [db (type 1)]
      ~@(View. db)))

; View is a flow and it's instantiated on the other peer 
;   it -> View, and is cc/ifn?   (a missionary signal)
;   instantiated -> new
; the signal "View" is a constant, the value of the constant is a missionary flow

; When we say "what is View" what do we mean?

(p/def App (p/fn []
             ~@(let [View (p/fn [db] ~@(prn db))
                     db   (type 1)]
                 ~@(View. db))))

; how do you know in the dynamic case
; the problem is binding unification

(tests
  (p/def View (if control
                (p/fn [] ~@(let [db %1] (prn db)))
                (p/fn [])))
  (analyze {} '(p/fn []
                 ~@(let [db   (type 1)]
                     ~@(View. db)))) :=
  _)

(tests
  (analyze {} '(p/fn []
                 ~@(let [View (p/fn [db] ~@(prn db))
                         db   (type 1)]
                     ~@(View. db)))) :=
  [[:constant
    [:target
     [:output [:apply [:global :clojure.core/prn] [:input]] [:nop]]
     [:output [:pub [:input] [:pub [:input] [:bind 1 1 [:bind 0 2 [:variable [:sub 2]]]]]] [:input]]]]
   [:target
    [:output
     [:pub
      [:constant [:pub [:node 0] [:output [:sub 1] [:input]]]]
      [:pub [:apply [:global :clojure.core/type] [:literal 1]] [:output [:sub 2] [:output [:sub 1] [:source [:input]]]]]]
     [:nop]]
    [:literal nil]]]


  (def client (first *1))                                   ; purple
  (def server (second *2))                                  ; red

  (emit (comp symbol str) client) :=
  `(r/peer
     2 0 0 0 0 0 0
     (fn [~'nodes]
       (r/steady
         (r/constant
           0 3 1 0 2 1 1
           (fn [~'nodes]
             (do
               (r/target
                 0 1 0 0 0 0 1
                 (fn [~'nodes]
                   (do (r/output 0 (r/latest-apply (r/steady prn) (r/input 0)))
                       nil)))
               (do (r/output
                     0
                     (let [~'pub0 (r/signal 0 (r/input 0))]
                       (let [~'pub1 (r/signal 1 (r/input 1))]
                         (let [~'nodes (assoc ~'nodes 1 ~'pub1)]
                           (let [~'nodes (assoc ~'nodes 0 ~'pub0)]
                             (r/variable 0 ~'nodes ~'pub0))))))
                   (r/input 2))))))))


  (emit (comp symbol str) server) :=
  `(r/peer
     1 0 1 0 0 0 0
     (fn [~'nodes]
       (do
         (r/target
           0 1 0 1 2 0 3
           (fn [~'nodes]
             (do
               (r/output
                 2
                 (let [~'pub0 (r/signal 0 (r/steady         ; pub0 = View
                                            (r/constant
                                              0 1 0 0 1 0 1
                                              (fn [~'nodes]
                                                ; pub0 = db
                                                (let [~'pub0 (r/signal 0 (nth ~'nodes 0))]
                                                  (do (r/output 0 ~'pub0) (r/input 0)))))))]
                   (let [~'pub1 (r/signal 1 (r/latest-apply (r/steady type) (r/steady (quote 1))))]
                     (do (r/output 0 ~'pub0) (do (r/output 1 ~'pub1) (do (r/source 0 ~'nodes) (r/input 0)))))))
               nil)))
         (r/steady (quote nil))))))

#?(:cljs
   (tests
     "client/server transfer, pure functional!"
     (def !x (atom 0))
     (def dispose ((p/client (p/main (tap (App.))))
                   js/console.log js/console.error))
     % := "#object[Number]"
     (swap! !x inc)
     % := "java.lang.Long"                                  ; holy cow
     (dispose)))


(comment
  #?(:clj (def server (@(requiring-resolve 'devkit/main) :main `main)))
  #?(:cljs (shadow.cljs.devtools.api/repl :app))
  (tests (pr-str (type 1)) := "#object[Number]"))
