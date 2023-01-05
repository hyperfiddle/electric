(ns dustin.y2022.forms.circuit-summation
  (:require #?(:clj [datomic.client.api :as d])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.forms.circuit-summation)))

(p/def rec!)

(defmacro summation
  "Σ[t=0..T] f(t)Δt
   where t = dbval
         f(t)Δt = the app"
  [rf [name x0] & body]
  `(let [!t# (atom ~x0) ~name (p/watch !t#)]
     (binding [rec! (fn [Δt] (swap! !t# ~rf Δt))] ; advance time, busy now true
       (do ~@body))))

(defn with [db tx] (:db-after (hf/with db tx))) ; p/wrap

(p/defn App [db e]
  (let [record (d/pull db label-form-spec cobbblestone)]

    (dom/dl
      (dom/dt (dom/text "name"))
      (dom/dd (let [v (ui/input (:label/name record) #_hf/loading)]
                (p/server (p/wrap (rec! [[:db/add e :label/name v]])))))

      (dom/dt (dom/text "sortName"))
      (dom/dd (let [v (ui/input (:label/sortName record) #_hf/loading)]
                (p/server (p/wrap (rec! [[:db/add e :label/sortName v]]))))))
    ))

(p/defn Page [db e]
  ; don't lose the stage on rebase!
  (summation with [db db] (App. db e)))


;(p/defn Page [db e]
;  (let [!db (atom db) db (p/watch !db)]
;    (binding [rec! (fn [tx] (swap! !db with tx))] ; advance time
;      (App. db e))))
