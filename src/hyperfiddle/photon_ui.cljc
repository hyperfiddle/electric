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
      (map (fn [events] (if (instance? Failure events) events (doall (sequence xf events))))
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
          (if (instance? Failure input)
            (rf result input)
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
  (dedupe-by first [{:a 1} {:a 1, :b 2}, {:a 1} {:a 2}])
  := [{:a 1} {:b 2} {:a 2}])

(defn check-interpretable! [x]
  (if-not (or (map? x) (every? vector? x))
    (do (log/error "Semicontroller expects a vector of events. Received:" (pr-str x))
      nil)
    x))

(defmacro interpreter [event-tags f & body]
  `(->> (p/fn [] ~@body)
        (m/eduction #_cat (dedupe-by first) (interpreter* ~event-tags ~f))
        (m/reductions {} nil)
        (m/relieve {})
        (new)))

(defmacro semicontroller
  "Act like a switch, prevent input to flow forward when in `open` state.
  Open or closed state is toggled by a boolean event of `event-tag`, produced by the `Body` reactive function."
  [event-tag input Body]
  `(let [!switch# (atom true)]
     (->> (p/fn [] (check-interpretable! (new ~Body (new (m/eduction (filter (partial deref' !switch#)) (p/fn [] ~input))))))
          (m/eduction #_cat (dedupe-by first) (interpreter* #{~event-tag} (partial set-switch! !switch#))
                      (filter seq))
          (m/reductions {} nil)
          (new))))

(tests
  (def !event (atom nil))
  (def !input (atom 0))
  (let [dispose (p/run (semicontroller :switch/set (new (m/watch !input))
                         (p/fn [input]
                           (! input)
                           (p/watch !event))))]
    % := 0
    (swap! !input inc)
    % := 1
    (reset! !event [[:switch/set false]])
    (swap! !input inc)
    % := ::rcf/timeout
    (rcf/set-timeout! 1200) ;; rcf issue, extend timeout
    (reset! !event [[:switch/set true]])
    (swap! !input inc)
    % := 3
    (dispose)))


(defn return-then-run! [v f]
  (m/ap (m/amb v (do (f) (m/amb)))))

(defmacro impulse [ack Callback >xs]
  `(let [val# (p/impulse ~ack ~>xs)]
     (when (some? val#)
       (new ~Callback val#))))

(defn merge-flows
  "Produces a flow returning values of both flows as soon as they are available."
  [fa fb]
  (->> (m/ap (m/amb= (m/?< fa) (m/?< fb)))
    (m/reductions {} nil)
    (m/relieve {})))

(defmacro auto-impulse [cancel-sym Callback >xs]
  `(let [!ack# (atom false)
         val#  (p/impulse ::down (new (merge-flows (p/fn [] (p/watch !ack#)) (p/fn [] ~cancel-sym))) ~>xs) ; letrec?
         [result# ack#] (when (not= ::down val#) [(new ~Callback val#) ::ack])] ; DG: pending source is here
     (when ack# (swap! !ack# not)) ; don’t rely on callback result, it might be `nil`
     result#))

(defn has-ns?
  "State if a `named` value (keyword or symbol) has such namespace `ns`.
  `ns` can be be a string, or a non-namespaced keyword or symbol."
  [ns named]
  {:pre [(or (string? ns) (simple-ident? ns))]}
  (= (name ns) (namespace named)))

(defn select-ns
  "Like `select-keys` but select all namespaced keys by ns."
  [ns map]
  (into (empty map) (filter (fn [[k _v]] (has-ns? ns k))) map))

(defn signals [props] (->> props
                        (select-ns :hyperfiddle.photon-ui)
                        keys
                        (filter #(str/ends-with? (name %) "-event"))
                        set))

(defn signal->event [sig] (str/replace (name sig) #"-event$" ""))

(defn gen-event-handlers
  ([props] (gen-event-handlers nil props {}))
  ([props transducers]
   (gen-event-handlers nil props transducers))
  ([cancel-sym props transducers]
   (map (fn [signal]
          (let [callback       (get props signal)
                [ack callback] (if (vector? callback) callback [nil callback])
                xf             (get transducers signal)
                event          (if (= ::keychord-event signal)
                                 `(dom/>keychord-events ~(::keychords props))
                                 `(dom/>events ~(signal->event signal) ~xf))]
            (if (some? ack)
              `[~signal (impulse ~ack ~callback ~event)]
              `[~signal (auto-impulse ~cancel-sym ~callback ~event)])))
     (signals props))))

(defn parse-props
  ([valuef props transducers]
   (parse-props valuef props transducers nil))
  ([valuef props transducers cancel-sym]
   (assert (map? props))
   [(valuef props nil)
    (gen-event-handlers cancel-sym props transducers)
    (let [dom-props (select-ns :hyperfiddle.photon-dom props)]
      (if-let [type (::type props)]
        (assoc dom-props :type type)
        dom-props))]))

(defmacro checkbox
  ([] `(checkbox {}))
  ([props]
     (let [[value events props'] (parse-props ::value props {})
           auto-value            (gensym "value_")]
       `(dom/bubble
          (let [~auto-value ~value]
            (dom/input (p/forget (dom/props ~props'))
              (p/forget (dom/props {:type          :checkbox
                                    :checked       ~auto-value
                                    :indeterminate (nil? ~auto-value)}))
              (into [[::value (dom/events "change" (map (dom/oget :target :checked)) ~auto-value)]]
                [~@events])))))))

(defmacro element [tag props & body]
  (let [[_ events props] (parse-props (constantly nil) props {})]
    `(dom/bubble
       (~tag (p/forget (dom/props ~props))
        (into [(do ~@body)]           ; coerced to a map
          [~@events])))))

(defmacro button [props & body]
  `(element dom/button ~props ~@body))

(defn format-num [format-str x] #?(:cljs (if format-str
                                           (format format-str x)
                                           (pr-str x))))
(defn parse-num [x] #?(:cljs (-> (js/parseFloat x) (* 100) (js/Math.round) (/ 100))))
(defn is-num? [x] #?(:cljs (not (js/isNaN x))))
(def parse-input (comp (map (dom/oget :target :value)) (map parse-num) (filter is-num?)))

(defmacro numeric-input [props]
  (let [[value events props'] (parse-props ::value props {})
        auto-value            (gensym "value_")]
    `(dom/bubble
       (semicontroller
         ::focused ~value
         (p/fn [~auto-value]
           (dom/input (p/forget (dom/props ~props'))
             (p/forget (dom/props {:value (format-num ~(::format props) ~auto-value)
                                   :type  :number})) ;; TODO should it pulse?
             (into [[::focused (not (new (dom/focus-state dom/node)))]
                    [::value (dom/events "input" parse-input ~auto-value)]]
               [~@events])))))))

(defmacro input
  ([] `(input {}))
  ([props & body]
   (case (::type props)
     :number   `(numeric-input ~props)
     :checkbox `(checkbox ~props)
     (let [[value events props'] (parse-props ::value props {})
           auto-value            (gensym "value_")]
       `(dom/bubble
          (semicontroller
            ::focused ~value
            (p/fn [~auto-value]
              (dom/input (p/forget (dom/props ~props'))
                (p/forget (dom/props {:value ~auto-value})) ;; TODO should it pulse?
                (into [(do ~@body)
                       [::focused (not (new (dom/focus-state dom/node)))]
                       [::value (dom/events "input" (map (dom/oget :target :value)) ~auto-value)]]
                  [~@events])))))))))

(defn- index-of [vec val] (.indexOf vec val))

(defn parse-select-value [options]
  (fn [event]
    (get options (parse-num (dom/oget event :target :value)))))

;; (defn parse-select-value [options] (comp (map (dom/oget :target :value)) (map parse-num) (map (partial get options))))

(defmacro select [props]
  (let [auto-options          (gensym "options_")
        [value events props'] (parse-props ::value props {::change-event `(map (juxt identity (parse-select-value ~auto-options)))})
        options               (vec (::options props))
        auto-value            (gensym "value_")]
    `(let [~auto-options ~options
           ~auto-value   ~value
           selected#     (index-of ~auto-options ~auto-value) ; TODO accept a keyfn prop instead of comparing with `=`.
           ]
       (dom/bubble
         (dom/select (p/forget (dom/props ~props'))
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
           (into [[::value (dom/events "change" (map (parse-select-value ~auto-options)) ~auto-value)]]
             [~@events]))))))

(defmacro native-typeahead [props]
  (let [[value events props'] (parse-props ::value props {})
        auto-value            (gensym "value_")
        auto-list             (str (gensym "list_"))]
    `(dom/bubble
       (semicontroller
         ::focused ~value
         (p/fn [~auto-value]
           (let [res#     (into {}
                            (dom/input (p/forget (dom/props ~props'))
                              (p/forget (dom/props {:type  :search
                                                    :list  ~auto-list
                                                    :value ~auto-value}))
                              (into [[::value (dom/events "input" (map (dom/oget :target :value)) ~auto-value)]
                                     [::focused (not (new (dom/focus-state dom/node)))]]
                                [~@events])))
                 needle#  (::value res#)
                 options# (when-let [options# ~(::options props `(p/fn [_#] nil))]
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
