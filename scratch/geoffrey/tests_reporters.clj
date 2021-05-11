(ns geoffrey.tests-reporters
  (:require [clojure.stacktrace :as stack]
            [clojure.test :as t]
            [geoffrey.tests-utils :refer [pprint testing-vars-str]]))

(defmethod t/report :pass [m]
  (t/with-test-out
    (t/inc-report-counter :pass)
    (if (:dots (:config m))
      (print "✅")
      (let [{:keys [expected actual message]} m
            [a b]                             (if (sequential? actual)
                                                (rest actual)
                                                [expected actual])]
        (print "✅ ")
        (pprint a)
        (print " := ")
        (pprint b)
        (print (str "  " (testing-vars-str m)))
        (prn)
        (when message
          (println message))))))

(defmethod t/report :fail [m]
  (t/with-test-out
    (t/inc-report-counter :fail)
    (prn)
    (print "🔥 ")
    (println (str (testing-vars-str m) " "))
    (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (prn)
    (pprint (:expected m))
    (prn)
    (print " := ")
    (prn)
    (pprint (:actual m))
    (prn)
    (prn)))

(defmethod t/report :error [m]
  (t/with-test-out
    (t/inc-report-counter :error)
    (prn)
    (print "❌ ")
    (print (str (testing-vars-str m) " "))
    (prn)
    (when (seq t/*testing-contexts*) (println (t/testing-contexts-str)))
    (when-let [message (:message m)] (println message))
    (pprint (:expected m))
    (prn)
    (print " := ")
    (prn)
    (prn)
    (let [actual (:actual m)]
      (if (instance? Throwable actual)
        (stack/print-cause-trace actual t/*stack-trace-depth*)
        (pprint actual)))
    ))


