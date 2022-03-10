(require '[hyperfiddle.photon :as p])

(def page-size 100)

(p/defn infinite-table [{:keys [query-fn row-fn]}]
  (p/client
    (let [offset (dom/input)]
      (p/for [e (query-fn offset)]
        (row-fn e)))))

(p/client
  (binding [dom/parent (js/document.getElementById "root")]
    (infinite-table
      {:query-fn
       (p/fn [offset]
         (p/server
           (->>
             (d/q '[:find [?e ...] :in $ :where [?e :person/name]] *db*)
             (drop (* page-size offset))
             (take page-size))))
       :render-fn
       (p/fn [x]
         (p/client
           (let [{:keys [person/name
                         person/email]} (p/server (d/entity *db* e))]
             (dom/tr (dom/td name) (dom/td email)))))})))