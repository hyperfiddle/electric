(ns user.form-typeahead
  (:require [missionary.core :as m]
            [hfdl.lang :as h :refer [defnode vars local2]]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.q2 :refer [q nav hfql exports]]
            [hyperfiddle.photon-dom :as d]
            [user.gender-shirt-size :refer [shirt-sizes]])
  #?(:cljs (:require-macros [user.form-typeahead])))

(defnode typeahead-select [parent entity]
  (let [div (d/element "div")
        input (doto (d/element "input")
                (d/set-attribute! "type" "text"))
        results (doto (d/element "div")
                  #_(d/set-visible! (d/focused? input)))]
    ~(d/mount parent div)
    ~(d/mount div input)
    ~(d/mount div results)
    ~@(h/for [e (shirt-sizes ~(nav entity :dustingetz/gender :db/ident)
                  ~@(-> ~(m/relieve {} (d/events input d/input-event))
                      d/event-target d/get-value))]
        (let [id ~(nav e :db/ident)]
          ~@~(d/mount results
               (doto (d/element "div")
                 (d/set-text-content! (name id))))))))

;(let [gender ...]
;  (typeahead-select _ _
;
;                    '(shirt-sizes ~(nav entity :dustingetz/gender :db/ident) needle)
;
;                    (node [needle] (shirt-sizes gender needle))))
