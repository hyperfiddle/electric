(ns hyperfiddle.examples.seven-guis.flight-booker
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [clojure.string :as str])
  #?(:cljs (:require-macros [hyperfiddle.examples.seven-guis.flight-booker :refer [ComboBox DateInput BookButton FlightBooker]])))

(def date-regexp #"^[0-9]{1,2}\.[0-9]{1,2}\.[0-9]{4}$")

(defn is-date? [x] (re-find date-regexp x))

(defn valid-date? [date] (and (= 3 (count date))
                              (nat-int? (nth date 0))
                              (nat-int? (nth date 1))
                              (nat-int? (nth date 2))))

;; (valid-date? (parse-date "11.22.3333a"))

(defn read-int [x] #?(:clj (Integer/parseInt x)
                      :cljs (js/parseInt x)))

(defn parse-date [x]
  (map read-int (some-> (re-find date-regexp x)
                        (str/split #"\."))))

(defn serialize-date [[m d y]]
  (str m "." d "." y))

(defn after-or-equal? [[m1 d1 y1] [m2 d2 y2]]
  (and (>= y2 y1)
       (>= m2 m1)
       (>= d2 d1)))


(photon/defn DateInput [disabled? default-value]
  (let [!value (atom default-value)]
    (dom/input
     (dom/attribute "value" (serialize-date default-value)) ;; uncontrolled, just init to a defalut value
     (dom/style {"background-color" (if (valid-date? ~(m/watch !value))
                                      "inherit"
                                      "red")})
     (dom/property "disabled" disabled?)
     (reset! !value
             ~(->> (dom/events dom/parent "keyup")
                   (m/eduction (map dom/event-target)
                               (map dom/get-value)
                               (map parse-date))
                   (m/reductions {} default-value)
                   (m/relieve {}))))))

(photon/defn ComboBox [default-value]
  (dom/select
   (dom/option (dom/attribute "value" "one-way") (dom/text "one-way flight"))
   (dom/option (dom/attribute "value" "return") (dom/text "return flight"))
   ~(->> (dom/events dom/parent "change")
         (m/eduction (map dom/event-target)
                     (map dom/get-value))
         (m/reductions {} default-value)
         (m/relieve {}))))

(defn log [m x] (prn m x) x)

(def default-date (parse-date "27.03.2014"))

(defn alert [x] #?(:cljs (js/alert x)))

(defn confirmation! [!flight-type !d1 !d2 click-event]
  (when (some? click-event)
    (alert
     (case @!flight-type
       "one-way" (str "You have booked a one-way flight on " (serialize-date @!d1))
       "return"  (str "You have booked a flight on " (serialize-date @!d1) ", returning on " (serialize-date @!d2))))))



(photon/defn BookButton [disabled?]
  (dom/button
   (dom/text "Book")
   (dom/property "disabled" disabled?)
   (log "init in " ~(->> (dom/events dom/parent "click")
                        (m/eduction (map (partial log "in 2")))
                        (m/reductions {} nil)
                        (m/relieve {})))))

(photon/defn FlightBooker []
  (let [!flight-type (atom nil)
        !d1          (atom nil)
        !d2          (atom nil)]
    (dom/div (dom/style {"display"        "grid"
                         "width"          "fit-content"
                         "grid-auto-flow" "columns"
                         "grid-gap"       "0.5rem"})
             (reset! !flight-type (photon/$ ComboBox "one-way"))
             (reset! !d1 (photon/$ DateInput false default-date))
             (reset! !d2 (photon/$ DateInput (not= "return" ~(m/watch !flight-type)) default-date))
             ;; FIXME BookButton is continuous, so changing any value after a
             ;; first click will display an alert, since the last click event is
             ;; re-sampled. It is probably related to how `do` works, since the
             ;; `disabled?` property might trigger a change in the
             ;; `dom/button`’s implicit `do`.
             (confirmation! !flight-type !d1 !d2
                           (photon/$ BookButton (or (not (after-or-equal? ~(m/watch !d1) ~(m/watch !d2)))
                                                    (not (valid-date? ~(m/watch !d1)))
                                                    (not (valid-date? ~(m/watch !d2)))))))))


(defn flight-booker []
  (photon/run
    (binding [dom/parent (dom/by-id "flight-booker")]
      (photon/$ FlightBooker))))

#?(:cljs (flight-booker))
