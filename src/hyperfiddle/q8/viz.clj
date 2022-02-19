(ns hyperfiddle.q8.viz
  (:require [dorothy.core :as dot]
            [dorothy.jvm :as djvm]
            [hyperfiddle.api :as hf]
            [hyperfiddle.q8.impl :as impl :refer [add-ref-deps dependencies parse reverse-deps]]))

(defn node-label [point]
  (let [{::impl/keys [role id form]} (meta point)
        id                           (if (impl/meta-keyword? id) (keyword id) id)
        alias                        (when-let [alias (`hf/as (meta form))]
                                       (str " as " alias))]
    (case role
      :collect  (str "[ … ]" alias)
      :traverse (str "{" id  " … }" alias)
      :call     (str form alias)
      (str "<font><i>"(name role) "</i>  " id alias "</font>"))))

(def default-arrowprops {:dir "back", :arrowtail "onormal"})

(defn viz!*
  ([deps] (viz!* deps "" default-arrowprops))
  ([deps title arrowprops]
   (let [deps  (reverse-deps deps)
         nodes (mapv (fn [point] [(keyword point) {:label  (node-label point)
                                                   :shape  "none"}]) (keys deps))
         links (mapcat (fn [[point deps]] (mapv (fn [child] [(keyword point) (keyword child) arrowprops]) deps)) deps)]
     (->> (concat nodes links)
          (dot/digraph {:id title})
          (dot/dot)
          (djvm/show!)))))

(defn viz! [kind form]
  (let [deps (add-ref-deps (dependencies (parse form)))]
    (case kind
      :let   (viz!* deps "Let dependencies" default-arrowprops)
      :scope (viz!* (reverse-deps deps) "Dynamic scope flow" {}))))


(comment

  (viz! '[{:user/gender [(:db/ident . ::hf/as gender)]}
          {(:user/shirt-size . ::hf/options (shirt-sizes gender)) [:db/ident]}])

  (viz! '[{:user/gender [:db/ident]}
          {:user/shirt-size [:db/ident]}])

  (viz!* (add-ref-deps (dependencies (parse [{:user/gender [:db/ident]}
                                             {:user/shirt-size [:db/ident]}]))))

  )
