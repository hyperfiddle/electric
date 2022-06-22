(ns hyperfiddle.ui2
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [missionary.core :as m]
            [hyperfiddle.logger :as log]
            [clojure.string :as str]
            #?(:cljs [goog.string.format])
            #?(:cljs [goog.string :refer [format]]))
  #?(:cljs (:require-macros [hyperfiddle.ui2 :refer [interpreter semicontroller component input]]))
  (:import (hyperfiddle.photon Pending Remote)))

(comment
  (rcf/enable!))

;; events looks like:
;; - [:db/add e a v]
;; - [:dom.input/focus true]
;; - [:browser/navigate url]
(defn interpret [event-tags f]
  (letfn [(matches? [event] (and (vector? event)
                              ((set event-tags) (first event))))]
    (fn [xf]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (if (matches? input)
           (do (f input) result)
           (xf result input)))))))

(tests
  "Unmatched events passes through"
  (sequence (interpret #{:dom.input/focus} (fn [_])) [[:browser/navigate "url"]])
  := '([:browser/navigate "url"]))

(tests
  "xf is called with matched events"
  (def !focus (atom false))
  (sequence (interpret #{:dom.input/focus} (fn [[_ focus?]] (reset! !focus focus?)))
    [[:browser/navigate "url"]
     [:dom.input/focus true]])
  := '([:browser/navigate "url"])
  (deref !focus) := true)

(defn set-switch! [!switch [event-tag open?]]
  (reset! !switch (boolean open?)))

(defn deref' [!atom & _] (deref !atom))

;; TODO defeated by z/instant?
(defn dedupe-n "Like `dedupe` but deduplicates individual values of a sequential collection, position-wise."
  ([]
   (fn [rf]
     (let [pv (volatile! ::init)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [prior @pv]
            (vreset! pv input)
            (if (= ::init prior)
              (rf result (seq input))
              (let [changed (->> (map vector input prior)
                              (filter #(not= (first %) (second %)))
                              (map first))]
                (if (seq changed)
                  (rf result changed)
                  result)))))))))
  ([coll] (sequence (dedupe-n) coll)))

(tests
  (dedupe-n [[1 "2"] [1 "3"] [2 "3"]])
  := '((1 "2") ("3") (2)))

(defn check-interpretable! [x]
  (if-not (vector? x)
    (do (log/error "Semicontroller expects a vector of events. Received:" (pr-str x))
      nil)
    x))

(defmacro interpreter [event-tags f & body]
  `(->> (p/fn [] ~@body)
        (m/eduction (dedupe-n) cat (interpret ~event-tags ~f))
        (m/reductions {} nil)
        (m/relieve {})
        (new)))

(defmacro semicontroller
  "Act like a switch, prevent input to flow forward when in `open` state.
  Open or closed state is toggled by a boolean event of `event-tag`, produced by the `Body` reactive function."
  [event-tag input Body]
  `(let [!switch# (atom true)]
     (->> (p/fn [] (check-interpretable! (new ~Body (new (m/eduction (filter (partial deref' !switch#)) (p/fn [] ~input))))))
          (m/eduction (dedupe-n) cat (interpret #{~event-tag} (partial set-switch! !switch#)))
          (m/reductions {} nil)
          (new))))

(tests
  (def !event (atom nil))
  (def !input (atom 0))
  (with (p/run (semicontroller :switch/set (new (m/watch !input))
                 (p/fn [input]
                   (! input)
                   [(p/watch !event)])))
    % := 0
    (swap! !input inc)
    % := 1
    (reset! !event [:switch/set false])
    (swap! !input inc)
    % := ::rcf/timeout
    (rcf/set-timeout! 1200) ;; rcf issue, extend timeout
    (reset! !event [:switch/set true])
    (swap! !input inc)
    % := 3))

(defn signals [props] (->> props keys (filter #(str/starts-with? (name %) "on")) set))

(defn signal->event [sig] (str/replace (name sig) #"^on-" ""))

(defmacro component [f value-key value-val props & body]
  `(let [props# ~props]
     (semicontroller :focused ~value-val
                     (p/fn [value-val#]
                       (~f (p/forget (dom/props props#))
                        (when-let [k# ~value-key]
                          (p/forget (dom/props {~value-key value-val#})))
                        (into [[:focused (not (new (dom/focus-state dom/node)))]]
                              (into (p/for [sig# (signals props#)]
                                      (dom/events (signal->event sig#) (get props# sig#)))
                                    (do ~@body))))))))

(defmacro input [props]
  `(let [props#    ~props
         on-input# (get props# :on-input (map (dom/oget :target :value)))]
     (component dom/input :value (:value props#) (-> props# (dissoc :value)
                                                     (assoc :on-input on-input#)))))

(defn format-num [format-str x] #?(:cljs (if format-str
                                           (format format-str x)
                                           (pr-str x))))
(defn parse-num [x] #?(:cljs (-> (js/parseFloat x) (* 100) (js/Math.round) (/ 100))))
(defn is-num? [x] #?(:cljs (not (js/isNaN x))))
(def parse-input (comp (map (dom/oget :target :value)) (map parse-num) (filter is-num?)))

(defmacro numeric-input [props]
  `(let [props#    ~props
         on-input# (if-let [on-input# (get props# :on-input)]
                     (comp parse-input on-input#)
                     parse-input)]
     (component dom/input :value (format-num (:format props#) (:value props#))
                (-> props#
                    (assoc :type :number)
                    (dissoc :value :format)
                    (assoc :on-input on-input#))
                [(:value props#)])))

(defmacro button [props & body]
  `(component dom/button nil nil ~props ~@body))

(defmacro checkbox [props]
  `(let [props#    ~props
         on-input# (get props# :on-input (map (dom/oget :target :checked)))]
     (component dom/input :checked (:checked props#) (-> props# (dissoc :value)
                                                         (assoc :type :checkbox)
                                                         (assoc :on-input on-input#))
                [(:checked props#)])))

(defn- lookup [kf x xs] (first (filter #(= x (kf %)) xs)))

(defmacro select [props]
  `(let [props#     ~props
         options#   (vec (:options props#))
         value#     (:value props#)
         on-change# (get props# :on-change (comp (map (dom/oget :target :value))
                                                 (map parse-num)
                                                 (map (partial get options#))))]
     (component dom/select :value (:value props#) (-> props#
                                                      (dissoc :value :options)
                                                      (assoc :on-change on-change#))
                (p/for [[idx# option#] (map-indexed vector options#)]
                  (dom/option {:value idx#}
                              (when (= (:value option#) value#
                                       (dom/props {:selected true})))
                              (dom/text (:text option#))))
                [(lookup :value value# options#)])))

(defmacro typeahead [props]
  `(let [props#    ~props
         ;; value#    (:value props#)
         on-input# (get props# :on-input (comp (map (dom/oget :target :value))))
         list-id   (str (gensym))]
     (let [value#   (component dom/input :value (:value props#) (-> props#
                                                                    (dissoc :value :options)
                                                                    (assoc :on-input on-input#
                                                                           :list list-id
                                                                           :type :search)))
           options# (new (:options props#) value#)]
       (dom/datalist {:id list-id}
                     (p/for [option# options#]
                       (dom/option (dom/text (:text option#)))))
       value#)))
