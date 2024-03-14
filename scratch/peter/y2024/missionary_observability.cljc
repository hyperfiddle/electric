(ns peter.y2024.missionary-observability
  (:require [missionary.core :as m])
  (:import #?(:clj [clojure.lang IFn IDeref])
           [missionary Cancelled]))

(defprotocol Observable
  (info [_])
  (get-parent [_])
  (set-parent! [_ p]))

(defn wrap [nm flow]
  (let [p (object-array [nil])]
    (reify
      Observable
      (info [_] (prn :in nm))
      (get-parent [_] (aget p (int 0)))
      (set-parent! [_ pp] (aset p (int 0) pp))

      IFn
      (#?(:clj invoke :cljs -invoke) [this n t]
        (let [p (flow n t)]
          (reify
            IDeref
            (#?(:clj deref :cljs -deref) [_]
              (try @p
                   (catch Cancelled e (throw e))
                   (catch #?(:clj Throwable :cljs :default) e
                     (run! info (eduction (take-while some?) (iterate get-parent this)))
                     (throw e))))
            IFn
            (#?(:clj invoke :cljs -invoke) [_]
              (p))))))))

(defn latesto [f & flows]
  (let [lat (wrap (str "lastesto-" f) (apply m/latest f flows))]
    (run! #(set-parent! % lat) flows)
    lat))

(comment
  (def !x (atom 1))
  (def it ((m/reduce #(prn %2) nil (latesto inc (latesto dec (wrap "watch" (m/watch !x))))) #(prn :ok %) #(prn :ex %)))
  (swap! !x inc)
  (reset! !x nil)
  (it)
  )
