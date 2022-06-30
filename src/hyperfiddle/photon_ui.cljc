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
  (:import (hyperfiddle.photon Pending Failure)
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
          (if (instance? Failure input) ; HACK FIXME we should not see instances of Failure here
            (throw input)
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
                result))))))))
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

;; (defmacro suspense [& body]
;;   `(let [!pending# (atom nil)]
;;      (dom/div {:data-hf-pending (p/watch !pending#)}
;;               (try (let [res# (do ~@body)]
;;                      (new (return-then-run! res# (partial reset! !pending# nil))))
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
           (new (return-then-run! res# (partial reset! !pending# nil))))
         (catch Pending _
           (reset! !pending# true)
           nil)
         (catch Cancelled _))))

(defn return-then-run! [v f]
  (m/ap (m/amb v (do (f) (m/amb)))))

(defmacro impulse [ack F >xs]
  `(let [val# (z/impulse ~ack ~>xs)]
     (do (prn "impulse val# is" val#)
         (new ~F val#))))

(defmacro auto-impulse [Ack >xs]
  `(let [!ack# (atom 0)
         val#  (z/impulse (p/watch !ack#) ~>xs)]
     (when (some? val#)
       (let [res# (new ~Ack val#)]
         (new (return-then-run! res# (partial swap! !ack# inc)))))))

(defn event
  "Take an arbitrary key and arbitrary value, the resulting event (a pair) will be
  returned by the ui component."
  [key value] ^{:tag ::event} [key value])
(defn event? [x] (= ::event (:tag (meta x))))

(defn gen-event-handlers
  ([props] (gen-event-handlers props {}))
  ([props transducers]
   (map (fn [signal]
          (case signal
            :on-keychord (let [callback                (get props signal)
                               [ack keychord callback] (case (count callback)
                                                         2 [nil (first callback) (second callback)]
                                                         3 callback)]
                           (if (some? ack)
                             `(impulse ~ack ~callback (dom/>keychord-events ~keychord))
                             `(auto-impulse ~callback (dom/>keychord-events ~keychord))))
            (let [callback       (get props signal)
                  [ack callback] (if (vector? callback) callback [nil callback])
                  xf             (get transducers signal)]
              (if (some? ack)
                `(impulse ~ack ~callback (dom/>events ~(signal->event signal) ~xf))
                `(auto-impulse ~callback (dom/>events ~(signal->event signal) ~xf))))))
     (signals props))))

(defn parse-props [valuef props transducers]
  (assert (map? props))
  (let [sigs (signals props)]
    [(valuef props)
     (gen-event-handlers props transducers)
     (apply dissoc props :value sigs)]))

(defmacro input [props]
  (let [[value events props'] (parse-props :value props {})
        auto-value            (gensym "value_")]
    `(into {}
       (semicontroller
         ::focused ~value
         (p/fn [~auto-value]
           (dom/input (p/forget (dom/props ~props'))
             (p/forget (dom/props {:value ~auto-value})) ;; TODO should it pulse?
             (into ~(merge (when (::value props)
                             {:value `(new ~(get props ::value `(p/fn [x#] x#))
                                        (dom/events "input" (map (dom/oget :target :value)) ~auto-value))})
                      `{::focused (not (new (dom/focus-state dom/node)))})
               (filter event? [~@events]))))))))

(defmacro checkbox [props]
  (let [[value events props'] (parse-props :checked props {})
        auto-value            (gensym "value_")]
    `(let [~auto-value ~value]
       (dom/input (p/forget (dom/props ~props'))
         (p/forget (dom/props {:type    :checkbox
                               :checked ~auto-value}))
         (into ~(merge {}
                  (when (::value props)
                    {:value `(new ~(get props ::value `(p/fn [x#] x#))
                               (dom/events "input" (map (dom/oget :target :checked)) ~auto-value))}))
           (filter event? [~@events]))))))

(defmacro element [tag props & body]
  (let [[_ events props] (parse-props (constantly nil) props {})]
    `(let []
       (~tag (p/forget (dom/props ~props))
        (into (safe-body (do ~@body))   ; coerced to a map
          (filter event? [~@events]))))))

#_
(defmacro element [tag props & body]
  `(let [props# ~props
         props# (-> (default props# :on-click (p/fn [x] x)))]
     (~tag (p/forget (dom/props props#))
      (into (safe-body (do ~@body))
        (p/for [sig# (signals props#)]
          (let [res# (auto-impulse (get props# sig#) (dom/>events (signal->event sig#)))]
            (when (vector? res#) res#)))))))

(defmacro button [props & body]
  `(element dom/button ~props ~@body))

(defn format-num [format-str x] #?(:cljs (if format-str
                                           (format format-str x)
                                           (pr-str x))))
(defn parse-num [x] #?(:cljs (-> (js/parseFloat x) (* 100) (js/Math.round) (/ 100))))
(defn is-num? [x] #?(:cljs (not (js/isNaN x))))
(def parse-input (comp (map (dom/oget :target :value)) (map parse-num) (filter is-num?)))

(defmacro numeric-input [props]
  (let [[value events props'] (parse-props :value props {})
        auto-value            (gensym "value_")]
    `(into {}
       (semicontroller
         ::focused ~value
         (p/fn [~auto-value]
           (dom/input (p/forget (dom/props ~props'))
             (p/forget (dom/props {:value (format-num ~(:format props') ~auto-value)
                                   :type  :number})) ;; TODO should it pulse?
             (into ~(merge (when (::value props)
                             {:value `(new ~(get props ::value `(p/fn [x#] x#))
                                        (dom/events "input" parse-input ~auto-value))})
                      `{::focused (not (new (dom/focus-state dom/node)))})
               (filter event? [~@events]))))))))

(defn- index-of [vec val] (.indexOf vec val))

(defn parse-select-value [options] (prn "options" options) (comp (map (dom/oget :target :value)) (map parse-num) (map (partial get options))))

(defmacro select [props]
  (let [[value events props] (parse-props :value props {})
        options              (vec (:options props))
        props                (dissoc props :options)
        auto-value           (gensym "value_")
        auto-options         (gensym "options_")]
    `(let [~auto-options ~options
           ~auto-value   ~value
           selected#     (index-of ~auto-options ~auto-value) ; TODO accept a keyfn prop instead of comparing with `=`.
           ]
       (dom/select (p/forget (dom/props ~props))
         (p/forget (dom/props {:value ~auto-value}))
         (when (= -1 selected#)
           (dom/option {:disabled true
                        :selected true}
             (dom/text (:text ~auto-value))))
         (p/for [[idx# option#] (map-indexed vector ~auto-options)]
           (dom/option {:value idx#}
             (when (= selected# idx#)
               (dom/props {:selected true}))
             (dom/text (:text option#))))
         (into ~(merge {}
                  (when (::value props)
                    {:value `(new ~(get props ::value `(p/fn [x#] x#))
                               (dom/events "input" (parse-select-value ~auto-options) ~auto-value))}))
           (filter event? [~@events]))))))

(defmacro native-typeahead [props]
  (let [[value events props'] (parse-props :value props {})
        auto-value            (gensym "value_")
        auto-list             (str (gensym "list_"))]
    `(into {}
       (semicontroller
         ::focused ~value
         (p/fn [~auto-value]
           (let [res#     (dom/input (p/forget (dom/props ~props'))
                            (p/forget (dom/props {:type  :search
                                                  :list  ~auto-list
                                                  :value ~auto-value}))
                            (into ~(merge {:value `(new ~(get props ::value `(p/fn [x#] x#))
                                                     (dom/events "input" (map (dom/oget :target :value)) ~auto-value))}
                                     `{::focused (not (new (dom/focus-state dom/node)))})
                              (filter event? [~@events])))
                 needle#  (:value res#)
                 options# (when-let [options# ~(:options props `(p/fn [_#] nil))]
                            (new options# needle#))]
             (dom/datalist {:id ~auto-list}
               (p/for [option# options#]
                 (dom/option {:id (:value option#)} (dom/text (:text option#)))))
             (vec res#)))))))

;;;;;;;;;;;;;;;;;;
;; scratch zone ;;
;;;;;;;;;;;;;;;;;;

(comment
  (defn map-by
    "Create a map from a collection `xs`, indexed by `kf`."
    [kf xs] (into {} (map (juxt kf identity)) xs))

  (defn sorted-map-by*
    "Like `sorted-map-by`, but can compare on an arbitrary map value using `kf`."
    ;; Handy to present sorted inforamation (e.g. UI) without sacrificing direct access.
    ([kf m] (sorted-map-by* compare kf m))
    ([comparator kf m] (into (sorted-map-by (fn [x y] (comparator (kf (get m x)) (kf (get m y))))) m)))

  (tests
    (->> [{:id 0 :name "Charlie"}         ; reversed lexicographic order
          {:id 1 :name "Bob"}
          {:id 2 :name "Alice"}]
      (map-by :id)                        ; {0 {:id 0, :name "Charlie"}, 1 …, 2 …}
      (sorted-map-by* :name)              ; {2 {:id 2, :name "Alice"}, 1 …, 0 …}
      (vals)
      (map :name))
    := '("Alice" "Bob" "Charlie"))

  )
