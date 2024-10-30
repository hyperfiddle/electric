(ns contrib.measure
  #?(:clj (:import [clojure.lang IFn IDeref]))
  (:require [clojure.pprint :as pp]
    [contrib.triple-store :as ts]))

(def !store nil)
(defn init [] #?(:clj (alter-var-root #'!store (constantly (java.util.ArrayList.)))
                 :cljs (set! !store #js [])))
(defn now [] #?(:clj (System/nanoTime) :cljs (js/performance.now)))
(defn add! [e] (when !store #?(:clj (.add ^java.util.ArrayList !store e) :cljs (.push !store e))))
(def ->id (partial swap! (atom 0) inc))
(def ^:dynamic *parent* nil)

(defn ! [id nm p evt thunk]
  (let [uid (->id), base {:p p, :id id, :nm nm, :uid uid :evt evt}]
    (add! (assoc base :type :start, :t (now)))
    (try (thunk) (finally (add! (assoc base :type :end, :t (now)))))))

(defn measure
  ([nm flow]
   (fn [n t]
     (let [id (->id), p *parent*]
       (binding [*parent* id]
         (! id nm p :spawn
           (fn []
             (let [it (flow #(! id nm p :notify n) #(! id nm p :terminate t))]
               (reify
                 IFn (#?(:clj invoke :cljs -invoke) [_] (! id nm p :cancel it))
                 (#?(:clj invoke :cljs -invoke) [_ _] it)
                 IDeref (#?(:clj deref :cljs -deref) [_]
                          (! id nm p :transfer (fn [] (binding [*parent* id] @it)))))))))))))

(defn pp [] (run! prn !store))

(defn took [ts uid]
  (let [start-e (ts/find1 ts :uid uid, :type :start)
        end-e (ts/find1 ts :uid uid, :type :end)]
    (- (ts/? ts end-e :t) (ts/? ts start-e :t))))

(defn d2 [n] (pp/cl-format nil "~,2F" n))

(defn stats []
  (let [ts (reduce #(ts/add % (assoc %2 :db/id (->id))) (ts/->ts) !store)]
    (doseq [uid (->> ts :ave :uid keys set)]
      (let [nd (ts/->node ts (ts/find1 ts :uid uid, :type :start))
            t (took ts uid)]
        (when (= (:evt nd) :transfer)
          (println (:nm nd) (:id nd) (:evt nd) (d2 t)))))
    (doseq [e (ts/find ts :nm 'for, :evt :transfer, :type :start)]
      (let [nd (ts/->node ts e)
            t (took ts (:uid nd))
            in-t* (sequence (comp (map #(ts/? ts % :uid)) (distinct) (map #(took ts %))) (ts/find ts :p (:id nd), :evt :transfer))
            in-t (reduce + 0 in-t*)]
        (println (map d2 in-t*))
        (println (:nm nd) (:evt nd) 'full-time (d2 t) 'clean-time (d2 (- t in-t)) 'row-avg (d2 (/ in-t (count in-t*))))))
    ))

(comment
  (def ts [10 5 5 5 5 5 4 8 6 4 7 5 3 5 7 7 4 8 3 4 4 9 6 5 6 4 5 5 6 5 5 5 4 5 6 5 5 5 5 4 6 5 5 5 4 5 6 5 5 5 4 4])
  (double (/ (reduce + ts) (count ts)))
  #?(:cljs !store)
  #?(:cljs (init))
  #?(:cljs (pp))
  )
