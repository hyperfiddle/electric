(ns dustin.y2022.controls.temperature_cycle2
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros dustin.y2022.controls.temperature_cycle2)))

(defmacro with-cycle [[sym init] & body]
  `(let [!x# (atom ~init)
         ~sym (new (m/watch !x#))]
     (reset! !x# (do ~@body))))

(let [fetch (Button. "Fetch")
      resolve (Button. "Resolve")
      reject (Button. "Reject")
      retry (Button. "Retry")]
  (with-cycle [r {:state :idle}]
              (case (:state r)
                :idle (if (:active fetch)
                        (assoc r
                          :state :pending
                          :retries 0)
                        r)
                :loading (if (:active resolve)
                           (assoc r :state :success)
                           (if (:active reject)
                             (assoc r :state :failure)
                             r))
                :failure (if (:active retry)
                           (-> r
                               (assoc :state :loading)
                               (update :retries inc))
                           r)
                :success (reduced r))))

(defmacro with-state [[sym init] & body]
  `(let [!x# (atom ~init)
         ~sym (partial swap! !x#)]
     ~@body (new (m/watch !x#))))

(p/defn Button [text]
  (with-state [state! #{}]
    (dom/h [:button {:on-mousedown (fn [_] (state! conj :active))
                     :on-mouseup (fn [_] (state! disj :active))} text])))

(:active (Button. "OK")) ;; truthy when clicked


"temperature converter"
(defn celsius->farenheit [c] (+ (* c (/ 9 5)) 32))
(defn farenheit->celsius [f] (* (- f 32) (/ 5 9)))

(p/defn Input [value change!]
  (p/with-cycle [edit false]
                (p/with-state [edit! false]
                              (dom/h
                                [:input
                                 {:type     "number"
                                  :step     0.5
                                  :value    (if edit (.-value dom/node) (ui/format-num "%.2f" value))
                                  :on-input (fn [e] (change! (js/parseFloat (.-value (.-target e)))))
                                  :on-focus (fn [_] (edit! not))
                                  :on-blur  (fn [_] (edit! not))}]))))

(p/defn App []
  (p/client
    (dom/h [:h1 "Temperature Converter"])
    (p/with-cycle [temp 0]
                  (p/with-state [temp! 0]
                                (dom/h
                                  [:dl
                                   [:dt "Celcius"]
                                   [:dd (Input. temp #(temp! {} %))]
                                   [:dt "Farenheit"]
                                   [:dd (Input. (celsius->farenheit temp) #(temp! {} (farenheit->celsius %)))]])))))

"temperature converter, reactive var feedback"

(defn celsius->farenheit [c] (+ (* c (/ 9 5)) 32))
(defn farenheit->celsius [f] (* (- f 32) (/ 5 9)))

(p/defn Input [value change!]
  (p/feed false
          (dom/h false
                 [:input
                  {:type     "number"
                   :step     0.5
                   :value    (if p/back (.-value dom/node) (ui/format-num "%.2f" value))
                   :on-input (fn [e] (change! (js/parseFloat (.-value (.-target e)))))
                   :on-focus (fn [_] (dom/state! not))
                   :on-blur  (fn [_] (dom/state! not))}])))

(p/defn App []
  (p/client
    (dom/h nil [:h1 "Temperature Converter"])
    (p/feed 0
            (dom/h 0
                   [:dl
                    [:dt "Celcius"]
                    [:dd (Input. p/back #(dom/state! {} %))]
                    [:dt "Farenheit"]
                    [:dd (Input. (celsius->farenheit p/back) #(dom/state! {} (farenheit->celsius %)))]]))))





(def types (mapv (fn [k] {:text (name k) ::value k}) ["" ::admin ::district]))

(p/defn Index []
  (let [!search (atom "") search (p/watch !search)
        !type-idx (atom 0) type-idx (p/watch !type-idx)]
    (ui/input {::ui/input-event (p/fn [e] (reset! !search (.. e -target -value)))})
    (ui/select {::ui/value        (get types type-idx)
                ::ui/options      types
                ::ui/change-event (p/fn [[_e v]] (reset! !type-idx (.indexOf types v)))})
    (p/server
      (GridSheet. (index. search (::value (get types type-idx)) *db*) {}))))

(p/defn Index []
  (let [search (dom/h "" [:input {:on-input (fn [e] (dom/state! {} (.-value (.-target e))))}])
        type-idx (dom/h 0
                        [:select {:on-input (fn [e] (dom/state! {} (js/parseInt (.-value (.-target e)) 10)))}
                         (p/for [[i {:keys [text]}] (map-indexed vector types)]
                           (dom/h nil [:option {:value (str i)} text]))])]
    (p/server (gridsheet/GridSheet. (index. search (::value (nth types type-idx)) *db*) {}))))