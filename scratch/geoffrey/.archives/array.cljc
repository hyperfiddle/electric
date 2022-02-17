(ns hyperfiddle.array
  (:refer-clojure :exclude [array contains? aget aset])
  (:require [clojure.test :refer [deftest is testing]])
  #?(:clj (:import java.util.ArrayList)))

(defn apush [arr val]
  #?(:clj (.add arr val)
     :cljs (.push arr val)))

(defn array ^ArrayList
  (^ArrayList [] #?(:clj (ArrayList.)
                    :cljs #js[]))
  (^ArrayList [v & vals] #?(:clj (ArrayList. (cons v vals))
                            :cljs (let [arr (array)]
                                    (doseq [x vals]
                                      (apush arr x))))))

(defn length [^ArrayList arr]
  #?(:clj (.size arr)
     :cljs (.length arr)))

(defn aget [^ArrayList arr, ^long idx]
  #?(:clj (.get arr idx)
     :cljs (cljs.core/aget arr idx)))

(defn aset [^ArrayList arr, ^long idx, val]
  #?(:clj (.set arr idx val)
     :cljs (cljs.core/aset arr idx val)))

(defn acopy [^ArrayList arr]
  #?(:clj (ArrayList. ^ArrayList arr)
     :cljs (js/Array.from arr)))

(defn contains? [^ArrayList arr, x]
  #?(:clj (not= -1 (.indexOf arr x))
     :cljs (.includes arr x)))

(defn aremove [^ArrayList arr, ^long idx]
  ;; Type hints mendatory to dispatch to the proper index-based `.remove` impl
  #?(:clj (.remove arr idx)
     :cljs (.splice arr idx 1)))

(defn clear [^ArrayList arr]
  #?(:clj (doto arr
            (.clear)
            (.trimToSize))
     :cljs (set! (.-length arr) 0)))


(deftype Foo []) ; used for tests below

(deftest arrays
  (testing "Arrays should"
    (testing "be mutable"
      (let [arr (array 1 2 3)]
        (apush arr 4)
        (is (= [1 2 3 4] arr))))
    (testing "have a minimum of equality semantics"
      (is (= [] (array)))
      (is (= (apply array (range 10)) (apply array (range 10))))
      (is (= [[1] [2]] (array (array 1) (array 2)))))
    (testing "search elements by memory address"
      (let [x (->Foo) ; dosen’t have an equalTo impl, compared using Java’s `=`
            y (->Foo)]
        (is (= true (contains? (array x y) x)))
        (is (= false (contains? (array y) x)))))
    (testing "be iterable"
      (is (= [1 2 3] (mapv identity (array 1 2 3)))))
    (testing "provide index-based access"
      (let [arr (array 1 2 3)]
        (is (= 2 (aget arr 1)))
        (is (thrown? IndexOutOfBoundsException (aget arr 3)))
        (is (= [1 :b 3] (do (aset arr 1 :b) arr)))))
    (testing "copy while preserving values"
      (is (= [1 2 3] (acopy (array 1 2 3)))))
    (testing "resize on"
      (testing "push"
        (is (= [1] (doto (array) (apush 1)))))
      (testing "removal"
        (is (= [1 3] (doto (array 1 2 3)
                       (aremove 1)))))
      (testing "clearing"
        (is (= [] (doto (apply array (range 1000))
                    (clear))))))))

