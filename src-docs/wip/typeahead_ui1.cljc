(ns wip.typeahead-ui1
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui :as ui]
   [clojure.string :as str])
  #?(:cljs (:require-macros wip.typeahead-ui1)))

(def data
  #?(:clj
     [{:name "Alice B"}
      {:name "Bob C"}
      {:name "Claire D"}
      {:name "Daniel E"}
      {:name "Emily F"}]))

(defn query [needle]
  ;; uncomment to imitate server lag
  ;; #?(:clj (Thread/sleep 100))
  (filterv #(str/includes? (:name %) needle) data))

;; react hooks similiarity
(defmacro state [val] `(let [val# ~val, atm# (atom val#)] [atm# (p/watch atm#)]))

(p/defn Typeahead [{:keys [placeholder on-pick on-create template-fn value-fn autofocus?]}]
  (let [[!show-suggestions? show-suggestions?] (state autofocus?)
        [!input input] (state "")
        suggestions (when show-suggestions? (p/server (query input)))
        [!idx idx] (state 0)
        idx (mod idx (count suggestions))]
    (ui/input {::ui/input-event (p/fn [e] (reset! !input (.. e -target -value)))
               ::ui/focus-event (p/fn [_] (reset! !show-suggestions? true))
               ::ui/value input
               ::ui/keychords #{"enter" "tab" "esc" "up" "down"}
               ::ui/keychord-event (p/fn [e]
                                     (case (:identifier e)
                                       ("enter" "tab") (let [picked (get suggestions idx)]
                                                         (if picked
                                                           (do
                                                             (on-pick picked)
                                                             (reset! !input (value-fn picked)))
                                                           (on-create input)))
                                       ("up")          (swap! !idx dec)
                                       ("down")        (swap! !idx inc)
                                       ("esc")         (reset! !input "")))
               ::ui/blur-event (p/fn [_] (reset! !show-suggestions? false))
               ::dom/placeholder (or placeholder "")}
      (when autofocus? (.focus dom/node)))
    (ui/element dom/div {::dom/style {:display (if show-suggestions? "block" "none")}
                         ::ui/pending {::dom/style {:background-color "red"}}}
      (p/for [[i sug] (map-indexed vector suggestions)]
        (ui/element dom/div {::ui/click-event (p/fn [e]
                                                (.preventDefault e)
                                                (on-pick sug)
                                                (reset! !input (value-fn sug)))
                             ::dom/style {:background-color (when (= i idx) "gray")}}
          (template-fn. sug))))))

(p/defn App []
  (p/client
    (Typeahead. {:placeholder "HI" :on-pick println :template-fn (p/fn [sug] (:name sug)) :value-fn :name
                 :on-create (fn [value] (println "creating" value)) :autofocus? true})))
