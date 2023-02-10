(ns hyperfiddle.electric-datascript-test
  (:require [clojure.test :as t]
            [datascript.core :as d]
            [hyperfiddle.electric :as p]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [missionary.core :as m])
  (:import [hyperfiddle.electric Pending Failure]
           [missionary Cancelled]))


(def !conn (d/create-conn {}))
(d/transact! !conn [{:task/status :done :task/description "buy milk"}])

(defn query-todos [db] (d/q '[:find [?e ...] :where [?e :task/status]] db))

(tests
  (p/def db (p/watch !conn))
  (with (p/run (p/for [id (query-todos db)]
                 (let [{:keys [:task/status :task/description]} (d/entity @!conn id)]
                   (tap status)
                   (tap description))))
    % := :done
    % := "buy milk"))
