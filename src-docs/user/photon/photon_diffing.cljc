(ns user.photon.photon-diffing
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! %]])
  #?(:cljs (:require-macros user.photon.photon-diffing)))

(hyperfiddle.rcf/enable!)

(tests
  (def !order (atom {:account/email "alice@example.com" :order/items [{:item/sku 100 :order/count 2}]}))

  (def dispose
    (p/run
      (let [order (p/watch !order)
            {:keys [account/email
                    order/items]} order]
        (println "email: " (! email))
        (println "item count: " (! (count items)))
        (p/for-by :item/sku [{:keys [item/sku order/count]} items]
          (println "sku: " (! sku))
          (println "sku count: " (! count))))))

  "touch everything first time"
  % := "alice@example.com" ; email
  % := 1 ; item count
  % := 100 ; sku
  % := 2 ; sku count

  "point write"
  (swap! !order update-in [:order/items 0 :order/count] inc)
  % := 3 ; sku count

  "add another record, only touch what changed"
  (swap! !order update-in [:order/items] conj {:item/sku 101 :order/count 1})
  % := 2 ; item count
  % := 101 ; sku
  % := 1 ; sku count

  % := ::rcf/timeout ; assert no more came

  (dispose))
;✅✅✅✅✅✅✅✅✅