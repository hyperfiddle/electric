(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom :refer [h]]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))


(def data
  [{:name "Alice B"}
   {:name "Bob C"}
   {:name "Claire D"}
   {:name "Daniel E"}
   {:name "Emily F"}])

(defn query [needle]
  (filter #(str/includes? (:name %) needle) data))

(p/defn Typeahead [{:keys [placeholder on-pick on-create template-fn value-fn autofocus?]}]
  (p/with-state {:status      :idle
                 :index       0
                 :input       ""
                 :suggestions []}
                (let [{:keys [status index input suggestions]} p/state
                      index (mod index (count suggestions))]
                  (case status
                    :idle (do)
                    :search (p/change! assoc :suggestions (p/server (query input)))
                    :validation (do (if-some [picked (get suggestions index)]
                                      (do (on-pick picked)
                                          (p/change! assoc :input (value-fn picked)))
                                      (p/server (on-create input)))
                                    (p/change! assoc :status :idle :suggestions [])))
                  (dom/h :input {:type        "text"
                                 :value       input
                                 :placeholder (str placeholder)
                                 :on-input    (fn [e] (p/change! assoc :input (.. e -target -value)))
                                 :on-focus    (fn [_] (p/change! assoc :status :search))
                                 :on-blur     (fn [_] (p/change! assoc :status :idle))
                                 :on-keydown  (fn [e]
                                                (case (.-key e)
                                                  ("Enter" "Tab") (p/change! assoc :status :validation)
                                                  ("ArrowUp") (p/change! update :index dec)
                                                  ("ArrowDown") (p/change! update :index inc)
                                                  ("Escape") (p/change! assoc :input "")
                                                  (do)))}
                    (when autofocus?
                      (p/change! assoc :status :search)
                      (.focus dom/node)))
                  (dom/h :div {:style {:display (if (seq suggestions) "block" "none")}}
                    (p/for [[i sug] (map-indexed vector suggestions)]
                      (dom/h :div {:on-click (fn [e]
                                               (.preventDefault e) (on-pick sug)
                                               (p/change! assoc :input (value-fn sug)))
                                   :style    {:background-color (when (= i index) "gray")}}
                        (template-fn. sug)))))))




(p/defn ManagedInput [{:keys [::on-input ::pending]}]
  (p/with-state [::Idle]
                (let [[tag & [x]] p/state] ; data State = Idle | Pending String
                  (case tag
                    ::Idle (do)
                    ::Pending (let [_ (on-input. x)]
                                (p/change! {} [::Idle _])))
                  (dom/h :input {:on-input (fn [e] (p/transition! ::_->::Pending (.. e -target -value)))}
                    (when (= tag pending)
                      (dom/props pending))))))

(ManagedInput. {::on-input (p/fn [v] (p/server (query v)))
                ::pending {::dom/aria-busy true}})

; 1. No impulse pattern
; 2. the accidental loop (task/flow impedance) is represented in the Pending -> Pending loop and can be prevented since we've modeled it
; 3.


(p/defn ManagedCheckbox [{:keys [::on-check ::pending ::value]}]
  (p/with-state [::Idle value]
                (let [[tag & [x]] p/state] ; data State = Idle | Pending String
                  (case tag
                    ::Idle (do)
                    ::Pending (let [_ (on-input. x)]
                                (p/change! {} [::Idle _])))
                  (dom/h :checkbox {:on-input (fn [e] (p/transition! ::Idle->Pending (.. e -target -value)))
                                    :value value}
                    (when (= tag pending)
                      (dom/props pending))))))

(ManagedCheckbox. {::on-check (p/fn [e]
                                (let [status (:status (d/entity db id))]
                                  (p/server (new (p/task->cp (transact! [{:db/id id, :task/status status}]))))))
                   ::ui/pending {::dom/aria-busy true}
                   ::ui/value (= :done status)
                   ::dom/class "toggle"})
