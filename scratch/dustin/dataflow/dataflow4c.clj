(ns dustin.dataflow.dataflow4c
  "circa feb 1 2021"
  )

(comment

  ; Idea: it seems reasonable that any symbolic fn can be associated with a single peer

  (defn query! [needle] (d/q ...))
  (def query [& args] (m/via m/blk (apply query! args)))
  ;(def inc [& args] (m/via m/cpu (apply clojure.core/inc args)))

  [:table (>> (>> (render-row (inc ~(<< :db/id (query (<< :db/id (query needle))))))))]
  (configure {'query1 :pink
              'query2 :purple}
    (reactive-for [% :db/id (query1 needle)
                   % :db/id (query2 (inc ~%))]
      [:table (render-row ~%)]))

  ;[:table xs (>> (render-row (<< :db/id (query needle))))]
  (reactive-for [% :db/id (query needle)]
    [:table (render-row %)])


  (def render-row
    (ifn [m]
      [:tr m]))

  (deflow render-table [xs]
    [:table xs (>> (render-row xs))]
    #_[:table xs #_(>> (render-row ~(<< :db/id xs)))])

  (defn query [needle]
    (vec
      (filter (comp #(clojure.string/includes? % needle) ::name)
        [{:db/id "a" ::name "alice"}
         {:db/id "b" ::name "bob"}
         {:db/id "c" ::name "charlie"}])))

  (configure {'query :pink
              :db/id :both
              'render-row :white}
    (let [[needle] (<- >route)]
      (effect-async (partial swap! effects conj)
        (vector
          [:table xs (>> (render-row (<< :db/id (query needle))))]

          #_(render-table ~(<< :db/id (query needle)))
          #_(render-table (query needle needle2))
          (if (<- >open)
            (render-table ~(query "alice"))
            ::nothing)))))

  (ReactDOM/render ~(datomic.api/q ...))

  (via {`deref ...
        `unquote ...}
    (:require ReactDOM [hyperfiddle.ui :refer [render-row]] [datomic.api :as d])
    @(ReactDOM/render (>> @(render-row ~(<< :db/id @(datomic.api/q ...))))))


  @(ReactDOM/render (>> @(render-row ~(<< :db/id @(datomic.api/q ...)))))
  @(ReactDOM/render [:table (rfor [% :db/id @(datomic.api/q ...)] @(render-row ~%))])
  (ReactDOM/render [:table (rfor [% :db/id (datomic.api/q ...)] (render-row '%))])

  (ReactDOM/render (render-row '(datomic.api/q ...)))


  (defmacro remote [tag body])

  ; Proposal:
  ; write unbiased program and map names to place

  ; Two options
  ; 1) tag functions as remote (magic - can tag from outside)
  ; 2) introduce remote operator to mark arbitrary block as remote (explicit)

  ; not biased
  (let [[needle] (<- >route)]
    (effect-async (partial swap! effects conj)
      (vector
        (render-table ~(let [_ _] (query needle needle2)))
        (if (<- >open)
          (render-table ~(query "alice"))
          ::nothing))))

  ; same as above but multiple colors
  (configure {!0 :pink
              !1 :purple})

  ; white-biased
  (let [[needle] (<- >route)]
    (effect-async (partial swap! effects conj)
      (vector
        (render-table (remote :pink (let [] (query needle needle2))))
        (if (<- >open)
          (render-table (remote :pink (query "alice")))
          ::nothing))))

  ; 1) magic approach - configure color at function points (domain)
  (configure {'query (remote :pink)}                              ; cannot send arbitrary form, only specific fn points
    (let [[needle] (<- >route)]
      (effect-async (partial swap! effects conj)
        (vector
          (render-table (let [] (query needle needle2)))
          (if (<- >open)
            (render-table (query "alice"))
            ::nothing)))))



  (let [[needle] (<- >route)]
    (effect-async (partial swap! effects conj)
      (vector
        (render-table ~(let [needle2 (str needle "'")]
                         (query needle needle2 #_(f ~(f ~needle)))))
        (if (<- >open)
          (render-table ~(query "alice"))
          ::nothing))))

  ; how do we partition the program across the system
  ; 1) mark the remote partitions, many way
  ; 2) given the program and the markings, find valid partitions
  ; ..... valid is one where there is no remote call for each subdag

  ; ways to figure out the partition
  ; 1) automatic partition?
  ; 2) mark it with ~
  ; 3) metadata

  ;
  )
