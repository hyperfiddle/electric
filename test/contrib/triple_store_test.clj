(ns contrib.triple-store-test
  (:require [contrib.triple-store :as ts]
            [hyperfiddle.rcf :as rcf :refer [tests]]))

(tests
  (-> (ts/->ts) (ts/add {:db/id 1, :foo 2}) (ts/get-entity 1) :foo) := 2
  (-> (ts/->ts) (ts/add {:db/id 1, :foo 1}) (ts/add {:db/id 2, :foo 1}) :ave :foo (get 1)) := #{1 2}
  ;; (-> (ts/->ts) (ts/add {:db/id 1, :foo 2, :bar 2}) :vea (get 2) (get 1)) := #{:foo :bar}
  (-> (ts/->ts) (ts/add {:db/id 1, :foo 2, :bar 2}) (ts/get-entity 1) (select-keys [:foo :bar :baz])) := {:foo 2, :bar 2}

  (-> (ts/->ts) (ts/add {:db/id '_}) (ts/upd '_ :x (fnil inc 0)) (ts/upd '_ :x (fnil inc 0)) (ts/get-entity '_) :x) := 2

  (-> (ts/->ts) (ts/add {:db/id 1}) (ts/asc 1 :x 2) (ts/asc 1 :x 2) :ave :x (get 2)) := #{1}
  )
