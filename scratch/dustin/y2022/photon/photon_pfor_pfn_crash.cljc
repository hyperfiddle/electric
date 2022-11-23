(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]))

(hyperfiddle.rcf/enable!)

; Crashes white page no error
(dom/tbody {:style {:height (str max-height "px")}}
  (let [!!rows (vec (repeatedly page-size (partial atom nil)))]
    (println (get-in !!rows [0]))
    #_(dom/div {:style {:padding-top (str padding-top "px")}})
    (p/for [i (range page-size)]
      (dom/tr {:style {:position "fixed"
                       :margin-top (str (* row-height i) "px")
                       :height (str row-height "px")}} ; freeze layout

        (let [[a & as] (p/server cols)
              m (p/watch (get !!rows i))]
          (dom/td {:style {:padding-left (-> (::depth m) (* 15) (str "px"))}}
            (some-> (get m a) (new a)))
          (p/for [a as]
            (dom/td (some-> (get m a) (new a)))))))
    #_(dom/div {:style {:padding-bottom (str padding-bottom "px")}})

    (p/server
      (let [xs (->> rows (drop start-row) (take page-size))]
        (p/for-by first [[i [depth m]] (map vector (range) xs)]
          (p/client
            (reset! (get-in !!rows [i])
                    #_(p/fn [a] (p/server (Format. m a (a m))))
                    (-> (p/for [a (p/server cols)]
                          (p/fn [] (p/server (pr-str a) #_(Format. m a (a m)))))
                        (->> (zipmap (p/server cols)))
                        (assoc ::depth depth)))))))))

; Working:
(dom/tbody {:style {:height (str max-height "px")}}
  (let [!!rows (vec (repeatedly page-size (partial atom nil)))]
    (println (get-in !!rows [0]))
    #_(dom/div {:style {:padding-top (str padding-top "px")}})
    (p/for [i (range page-size)]
      (dom/tr {:style {:position "fixed"
                       :margin-top (str (* row-height i) "px")
                       :height (str row-height "px")}} ; freeze layout

        (let [[a & as] (p/server cols)
              F (p/watch (get !!rows i))]
          (dom/td {:style {:padding-left (-> (::depth m) (* 15) (str "px"))}}
            (some-> F #_(get m a) (new a)))
          (p/for [a as]
            (dom/td (some-> F #_(get m a) (new a)))))))
    #_(dom/div {:style {:padding-bottom (str padding-bottom "px")}})

    (p/server
      (let [xs (->> rows (drop start-row) (take page-size))]
        (p/for-by first [[i [depth m]] (map vector (range) xs)]
          ;(let [[a & as] cols])
          (p/client
            (reset! (get-in !!rows [i])
                    (p/fn [a] (p/server (Format. m a (a m))))
                    #_
                            (-> (p/for [a (p/server cols)]
                                  (p/fn [] (p/server (pr-str a) #_(Format. m a (a m)))))
                                #_[(p/fn [] (p/server (Format. m :db/ident (:db/ident m))))
                                   (p/fn [] (p/server (Format. m :db/valueType (:db/valueType m))))
                                   (p/fn [] (p/server (Format. m :db/cardinality (:db/cardinality m))))
                                   (p/fn [] (p/server (Format. m :db/unique (:db/unique m))))]
                                (->> (zipmap (p/server cols)))
                                (assoc ::depth depth)))))))))