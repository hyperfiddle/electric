(ns forms
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  #?(:cljs (:require-macros forms)))

(comment
  "Sketch of EAV forms"

  (defmacro form [e & body]
    (binding [hf/e e]
      (let [[tx1# tx2#] (do ~@body)]
        (p/server (hf/into-tx hf/schema tx1# tx2#)))))

  (defmacro field [a v & [props]]
    `(when-some [[v'#] (p/with-cycle [[v'# v#] [nil ~v]]
                         [(ui/input v# ~props (= v# v'#))
                          v#])]
       ; todo validate
       [[:db/add ~hf/e ~a v'#]]))

  (p/defn LabelForm [e]
    (dom/h1 "Change name for label: " (p/server (query-label-name hf/db e)))
    (form e
          (field :label/gid (p/server (pr-str (:label/gid (d/pull hf/db [:label/gid] e)))) {::dom/disabled true})
          (field :label/name (p/server (query-label-name hf/db e)) {}))))

; bubbling, see wip.demo-bubbles

;(defn map-commands [f xs] (into [] (comp (map f) (remove nil?)) xs))
;
;(defn interpret [commands]
;  (->> commands
;       (map-commands
;         (fn [[tag x]]
;           (case tag
;             :set-checked x
;             nil)))))
;
;(p/for [[tag x] commands]
;  (case tag :set-checked x nil))

(p/defn Field [x a props]
  (let [v (nav x a (a x))
        e (dx/identify x)] ; need record to infer identity unless coordinated ahead
    (try
      (dom/dt (name a))
      (dom/dd {} (ui/input {::ui/value v
                            ::ui/blur identity ; close the loop
                            ::dom/disabled true}))
      (catch ::ui/value [_ v]
        (throw [::hf/stage [[:db/add e a v]]]))
      (catch ::ui/blur [_ v]
        (println 'blur)
        nil))))

(p/defn Form [x]
  (dom/dl
    (Field. record :db/id {::ui/disabled true})
    (Field. record :label/name {})))

(p/with-cycle [stage []]
  (binding [hf/db (:db-after (hf/with secure-db stage))]
    (try
      (Form. cobbblestone)
      (catch ::hf/stage es ; all simultaneously active commands of this type
        (reduce into-tx (map ... es))
        ; merge them all together or swap them in one at a time?
        txn))))

