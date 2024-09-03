(ns hyperfiddle.kvs)

(defprotocol KVS
  (insert! [_ k v])
  (update! [_ k f])
  (remove! [_ k]))