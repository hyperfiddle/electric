(ns hyperfiddle.photon-ui
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [missionary.core :as m]
            [hyperfiddle.logger :as log]
            [clojure.string :as str]
            #?(:cljs [goog.string.format])
            #?(:cljs [goog.string :refer [format]]))
  #?(:cljs (:require-macros [hyperfiddle.photon-ui :refer [interpreter semicontroller input]]))
  (:import (hyperfiddle.photon Pending)
           (missionary Cancelled)))

(comment
  (rcf/enable!))

(declare dedupe-n)

;; events looks like:
;; - [:db/add e a v]
;; - [:dom.input/focus true]
;; - [:browser/navigate url]
(defn interpret [event-tags f]
  (let [event-tags (set event-tags)
        matches? (fn [event] (and (vector? event)
                                  ((set event-tags) (first event))))]
    (comp (map (fn [event]
                 ;; (prn "seen event" event)
                 (if (matches? event)
                   (do (f event)
                       ::nil)
                   event)))
          (remove #{::nil}))))

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

(defn interpreter* [event-tags f]
  (let [xf (interpret event-tags f)]
    (comp #_(map (fn [xs] (prn "=>" xs) xs))
          (map (fn [events] (doall (sequence xf events)))
               #_(partial sequence xf)))))

(tests
 "Unmatched events passes through"
 (sequence (interpreter* #{:dom.input/focus} (fn [_])) [[[:browser/navigate "url"]]])
 := '(([:browser/navigate "url"])))

(tests
 "xf is called with matched events"
 (def !focus (atom false))
 (sequence (interpreter* #{:dom.input/focus} (fn [[_ focus?]] (reset! !focus focus?)))
           [[[:browser/navigate "url"]
             [:dom.input/focus true]]])
 := '(([:browser/navigate "url"]))
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

(defn dedupe-by
  "Returns a lazy sequence removing consecutive duplicates in coll.
  Returns a transducer when no collection is provided."
  {:added "1.7"}
  ([f]
   (fn [rf]
     (let [pv (volatile! {})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [[nv new-events] (loop [seen (transient @pv)
                                       r    (transient [])
                                       xs   input]
                                  (if (seq xs)
                                    (let [x (first xs)
                                          k (f x)]
                                      (if (and (contains? seen k) (= (get seen k) x))
                                        (recur seen r (rest xs))
                                        (recur (assoc! seen k x) (conj! r x) (rest xs))))
                                    [(persistent! seen) (persistent! r)]))]
            (vreset! pv nv)
            (if (pos? (count new-events))
              (rf result (into (empty input) new-events))
              result)))))))
  ([f coll] (sequence (dedupe-by f) coll)))

(tests
  (dedupe-by key [{:a 1} {:a 1, :b 2}, {:a 1} {:a 2}])
  := [{:a 1} {:b 2} {:a 2}])

(defn check-interpretable! [x]
  (if-not (or (map? x) (every? vector? x))
    (do (log/error "Semicontroller expects a seq vector of events. Received:" (pr-str x))
      nil)
    x))

(defmacro interpreter [event-tags f & body]
  `(->> (p/fn [] ~@body)
        (m/eduction #_(dedupe-n) #_cat (dedupe-by key) (interpreter* ~event-tags ~f))
        (m/reductions {} nil)
        (m/relieve {})
        (new)))

(defmacro semicontroller
  "Act like a switch, prevent input to flow forward when in `open` state.
  Open or closed state is toggled by a boolean event of `event-tag`, produced by the `Body` reactive function."
  [event-tag input Body]
  `(let [!switch# (atom true)]
     (->> (p/fn [] (check-interpretable! (new ~Body (new (m/eduction (filter (partial deref' !switch#)) (p/fn [] ~input))))))
          (m/eduction #_(dedupe-n) #_cat (dedupe-by key) (interpreter* #{~event-tag} (partial set-switch! !switch#))
                      (filter seq))
          (m/reductions {} nil)
          (new))))

(tests
  (def !event (atom nil))
  (def !input (atom 0))
  (with (p/run (semicontroller :switch/set (new (m/watch !input))
                 (p/fn [input]
                   (! input)
                   (p/watch !event))))
    % := 0
    (swap! !input inc)
    % := 1
    (reset! !event {:switch/set false})
    (swap! !input inc)
    % := ::rcf/timeout
    (rcf/set-timeout! 1200) ;; rcf issue, extend timeout
    (reset! !event {:switch/set true})
    (swap! !input inc)
    % := 3))

(defmacro merge-events [& events]
  `(->> (p/fn [] (apply merge ~@events))
        (m/eduction (dedupe-by key)
                    (filter seq))
        (m/reductions {} nil)
        (m/relieve {})
        (new)))

(defn signals [props] (->> props keys (filter #(str/starts-with? (name %) "on")) set))

(defn signal->event [sig] (str/replace (name sig) #"^on-" ""))

(defn safe-body [xs]
  (cond (vector? xs) (into {} xs)
        (map? xs)    xs
        :else        {}))

(defn default [m k v]
  (if (contains? m k) m (assoc m k v)))

(comment
  (when-let [event# (z/impulse z/time (dom/>events (signal->event sig#)))]
    (let [res# (new (get props# sig#) event#)]
      (when (vector? res#) res#)))
  )

(defn do-and-ret [f v]
  (m/ap (m/amb v (do (f) (m/amb)))))

;; (defmacro suspense [& body]
;;   `(let [!pending# (atom nil)]
;;      (dom/div {:data-hf-pending (p/watch !pending#)}
;;               (try (let [res# (do ~@body)]
;;                      (new (do-and-ret (partial reset! !pending# nil) res#)))
;;                    (catch Pending _
;;                      (prn "Suspense pending …")
;;                      (reset! !pending# true)
;;                      nil)
;;                    (catch Cancelled _
;;                      (prn "cancelled"))))))

(defmacro suspense [Callback]
  `(let [!pending# (atom nil)
         effect# (p/fn [F] (p/forget (new F (p/watch !pending#))))]
     (try (let [res# (new ~Callback effect#)]
           (new (do-and-ret (partial reset! !pending# nil) res#)))
         (catch Pending _
           (reset! !pending# true)
           nil)
         (catch Cancelled _))))

(defmacro pending-impulse [Ack >xs]
  `(let [!pending# (atom 0)
         val#      (z/impulse (p/watch !pending#) ~>xs)]
     (try
       (when (some? val#)
         (let [res# (new ~Ack val#)]
           (new (do-and-ret (partial swap! !pending# inc) res#))))
       (catch Pending err
         (throw err)))))

(defmacro button [props & body]
  `(let [props# ~props
         props# (default props# :on-click (p/fn [x] [:click true]))]
     (:click (dom/button (p/forget (dom/props props#))
                         (into (safe-body (do ~@body))
                               (p/for [sig# (signals props#)]
                                 (let [res# (pending-impulse (get props# sig#) (dom/>events (signal->event sig#)))]
                                   (when (vector? res#) res#))))))))


(defmacro input [props]
  `(let [props# ~props
         props# (default props# ::on-change (p/fn [x] x))]
     (::value
      (into {} (semicontroller
                ::focused (:value props#)
                (p/fn [value#]
                  (dom/input (p/forget (dom/props (dissoc props# :value)))
                             (p/forget (dom/props {:value value#}))
                             (into {::value   value#
                                    ::focused (not (new (dom/focus-state dom/node)))}
                                   (p/for [sig# (signals props#)]
                                     (if (= ::on-change sig#)
                                         [::value (new (get props# ::on-change)
                                                       (dom/events "input" (map (dom/oget :target :value))
                                                                   value#))]
                                         (let [res# (pending-impulse (get props# sig#) (dom/>events (signal->event sig#)))]
                                           (when (vector? res#) res#))))))))))))

(defn format-num [format-str x] #?(:cljs (if format-str
                                           (format format-str x)
                                           (pr-str x))))
(defn parse-num [x] #?(:cljs (-> (js/parseFloat x) (* 100) (js/Math.round) (/ 100))))
(defn is-num? [x] #?(:cljs (not (js/isNaN x))))
(def parse-input (comp (map (dom/oget :target :value)) (map parse-num) (filter is-num?)))

(defmacro numeric-input [props]
  `(let [props# ~props
         value# (:value props#)]
     (into {} (semicontroller
               ::focused value#
               (p/fn [value#]
                 (dom/input (p/forget (dom/props props#))
                            (p/forget (dom/props {:value (format-num (:format props#) value#)
                                                  :type  :number}))
                            (into {::value   value#
                                   ::focused (not (new (dom/focus-state dom/node)))}
                                  (p/for [sig# (signals props#)]
                                    (if (= :on-change sig#)
                                      (let [res# (pending-impulse (get props# sig#) (dom/>events "input" parse-input))]
                                        (when (vector? res#) res#))
                                      (let [res# (pending-impulse (get props# sig#) (dom/>events (signal->event sig#)))]
                                        (when (vector? res#) res#)))))))))))

(defmacro checkbox [props]
  `(let [props# ~props
         props# (default props# ::on-change (p/fn [x] [::value x]))
         value# (::value props#)]
     (::value (dom/input (p/forget (dom/props props#))
                         (dom/props {:type    :checkbox
                                     :checked value#})
                         (into {::value value#}
                               (p/for [sig# (signals props#)]
                                 (if (= ::on-change sig#)
                                   (new (get props# ::on-change)
                                        (dom/events "change" (map (dom/oget :target :checked)) value#))
                                   (let [res# (pending-impulse (get props# sig#) (dom/>events (signal->event sig#)))]
                                     (when (vector? res#) res#)))))))))

(defn- index-of [vec val] (.indexOf vec val))

(defmacro select [props]
  `(let [props#    ~props
         props#    (default props# ::on-change (p/fn [x] x))
         options#  (vec (:options props#))
         value#    (:value props#)
         selected# (index-of options# value#) ; TODO accept a keyfn prop instead of comparing with `=`.
         ]
     (::value
      (into {} (dom/select (p/forget (dom/props props#))
                           (p/forget (dom/props {:value value#}))
                           (when (= -1 selected#)
                             (dom/option {:disabled true
                                          :selected true}
                                         (dom/text (:text value#))))
                           (p/for [[idx# option#] (map-indexed vector options#)]
                             (dom/option {:value idx#}
                                         (when (= selected# idx#)
                                           (dom/props {:selected true}))
                                         (dom/text (:text option#))))
                           (into [[::value   value#]]
                                 (p/for [sig# (signals props#)]
                                   (if (= ::on-change sig#)
                                     [::value (new (get props# ::on-change)
                                                   (dom/events "change" (comp (map (dom/oget :target :value))
                                                                             (map parse-num)
                                                                             (map (partial get options#)))
                                                               value#))]
                                     (let [res# (pending-impulse (get props# sig#) (dom/>events (signal->event sig#)))]
                                       (when (vector? res#) res#))))))))))

(defmacro native-typeahead [props]
  (let [list-id (str (gensym))]
    `(let [props# ~props
           props# (default props# ::on-change (p/fn [x] x))]
       (::value
        (into {} (semicontroller
                  ::focused (:value props#)
                  (p/fn [value#]
                    (let [res#     (into {} (dom/input (p/forget (dom/props props#))
                                                       (p/forget (dom/props {:type :search
                                                                             :value value#
                                                                             :list  ~list-id}))
                                                       (into [[::value   value#]
                                                              [::focused (not (new (dom/focus-state dom/node)))]]
                                                             (p/for [sig# (signals props#)]
                                                               (if (= ::on-change sig#)
                                                                 [::value (new (get props# ::on-change)
                                                                               (dom/events "input" (map (dom/oget :target :value))
                                                                                           value#))]
                                                                 (let [res# (pending-impulse (get props# sig#) (dom/>events (signal->event sig#)))]
                                                                   (when (vector? res#) res#)))))))
                          needle#  (::value res#)
                          options# (when-let [options# (:options props#)]
                                     (new options# needle#))]
                      (dom/datalist {:id ~list-id}
                                    (p/for [option# options#]
                                      (dom/option {:id (:value option#)} (dom/text (:text option#)))))
                      (vec res#)))))))))

