(ns hyperfiddle.q8.viz
  (:require [dorothy.core :as dot]
            [dorothy.jvm :as djvm]
            [hyperfiddle.api :as hf]
            [hyperfiddle.q8.impl :as impl :refer [add-ref-deps dependencies parse reverse-deps]]))


(defn viz!* [deps]
  (let [nodes (mapv (fn [dep] [(keyword dep) {:label  (pr-str (::node (meta dep)))
                                              :xlabel (str (`hf/as (meta (::node (meta dep)))))
                                              :shape  "none"}]) (keys deps))
        links (mapcat (fn [[dep deps]] (mapv (fn [child] [(keyword dep) (keyword child)]) deps)) deps)]
    (->> (concat nodes links)
         (dot/digraph)
         (dot/dot)
         (djvm/show!))))

(defn viz! [form] (viz!* (add-ref-deps (dependencies (parse form)))))


(comment

  (viz! '[{:user/gender [(:db/ident . ::hf/as gender)]}
          {(:user/shirt-size . ::hf/options (shirt-sizes gender)) [:db/ident]}])

  (viz! '[{:user/gender [:db/ident]}
          {:user/shirt-size [:db/ident]}])

  (viz!* (reverse-deps (add-ref-deps (dependencies (parse [{:user/gender [:db/ident]}
                                                           {:user/shirt-size [:db/ident]}])))))

  )
