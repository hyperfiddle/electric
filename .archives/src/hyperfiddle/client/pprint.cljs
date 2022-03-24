(ns hyperfiddle.client.pprint
  (:require [hyperfiddle.client.printer :as printer]))

(defn pprint-async
  "Like pprint, but run in another thread."
  ([o callback]
   (pprint-async o nil callback))
  ([o columns callback]
   (printer/pprint o columns callback)))
