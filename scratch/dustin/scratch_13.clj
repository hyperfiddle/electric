(ns leo.scope
  (:require [hyperfiddle.rcf :refer [tests]]
            [hfdl.lang :as h :refer [defnode vars local2 node thread]]
            [hyperfiddle.photon :as photon]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.api :as hf]
            [missionary.core :as m]
            [hyperfiddle.q2 :refer [hfql]]))

(declare inject gender shirt-sizes main nav)

(defnode typeahead-options)
(defnode typeahead-select
  (inject {d/parent d/div}
          (let [input d/input-text]
            ~@(h/for [e (inject {h/needle ~@(-> ~(m/relieve {} (d/events input d/input-event))
                                                d/event-target d/get-value)} typeahead-options)]
                     (let [id ~(nav e :db/ident)]
                       ~@~(doto d/div (d/set-text-content! (name id))))))))
(main
  (inject {gender            ~(nav dev/alice :dustingetz/gender :db/ident)
           typeahead-options (f)} typeahead-select))


(defnode foo
  (inject [db x]
          ))




; 1
(defnode typeahead-select [option]
  ~@(dom/div
      (let [needle (dom/input "")]
        (dom/select (for [x ~@($ options needle)]
                      (dom/option x))))))

(photon/main
  (binding [hf/*$* (datomic.api/db ...)
            dom/parent (js/document.querySelector "#root")]
    (typeahead-select (node [needle]
                        ~@(dom/div
                            ~@(datomic.api/q ... needle hf/*$*))))))


; 2
(defn typeahead-select [option]
  (dom/div
    (let [needle (dom/input "")]
      (dom/select (for [x (options needle)]
                    (dom/option x))))))

(photon/main
  (binding [hf/*$* (datomic.api/db ...)
            dom/parent (js/document.querySelector "#root")]
    (typeahead-select (fn [needle]
                        (datomic.api/q ... needle hf/*$*)))))

; 3
(defmacro typeahead-select [option]
  (dom/div
    (let [needle (dom/input "")]
      (dom/select (for [x (options needle)]
                    (dom/option x))))))

(photon/main
  (binding [hf/*$* (datomic.api/db ...)
            dom/parent (js/document.querySelector "#root")]
    (typeahead-select (fn [needle]
                        (datomic.api/q ... needle hf/*$*)))))

; linearized version
(defnode typeahead-select [parent0 option]
  (let [parent1 (dom/div parent0)
        parent2 (dom/input parent1 "")
        needle (dom/input-changes parent2)
        parent3 (dom/select parent2)
        parent4 (for [x ($ options needle)] (dom/option parent3 x))]))

; inject version
(defmacro element [type & body]
  `(inject {dom/parent (dom/create-element ~type)} ~@body))

(def options)
(def ^:dynamic db)
(defnode typeahead-select []
  (div
           (inject {needle (input (dom/input-changes))}
                   (select
                            (for [x options]
                              (option (dom/set-label x)))))))

(inject {options (datomic.api/q ... needle)} (typeahead-select))


















