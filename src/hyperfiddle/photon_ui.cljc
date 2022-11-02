(ns hyperfiddle.photon-ui
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [hyperfiddle.logger :as log]
            [clojure.string :as str]
            [contrib.data :as data]
            #?(:cljs [goog.string.format])
            #?(:cljs [goog.string :refer [format]]))
  #?(:cljs (:require-macros hyperfiddle.photon-ui))
  (:import (hyperfiddle.photon Pending Failure)
           (missionary Cancelled)))

(comment
  (rcf/enable!))

(defn apply-some "call f with arg iff arg is non-nil" [f arg] (some-> arg f))
(defn find-if-non-nil [m k]
  (when-let [entry (find m k)]
    (when (some? (val entry))
      entry)))

(defmacro handle "Calls f if body produces any of event-tags" [event-tags f & body]
  (let [event-map (gensym "event-map_")
        fsym      (gensym "f_")]
    `(let [events#    ~(if (= 1 (count body)) (first body) `(do ~@body))
           ~event-map (into {} events#)
           ~fsym         ~f]
       ~@(map (fn [tag] `(apply-some ~fsym (find-if-non-nil ~event-map ~tag))) event-tags)
       (remove (comp ~event-tags first) events#))))

(tests
  "Unmatched events passes through"
  (handle #{:dom.input/focus} (fn [_]) [[:browser/navigate "url"]])
  := '([:browser/navigate "url"]))

(tests
  "xf is called with matched events"
  (def !focus (atom false))
  (handle #{:dom.input/focus} (fn [[_ focus?]] (reset! !focus focus?))
    [[:browser/navigate "url"]
     [:dom.input/focus true]])
  := '([:browser/navigate "url"])
  (deref !focus) := true)

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

(defn signals [props] (->> props
                        (data/select-ns :hyperfiddle.photon-ui)
                        keys
                        (filter #(str/ends-with? (name %) "-event"))
                        set))

(defn signal->event [sig] (str/replace (name sig) #"-event$" ""))

(def xf-ignore-aria-disabled
  "Discard dom events performed on an aria-disabled element or a descendant of an
aria-disabled element.
  The dom attribute `disabled` is technically rigid and damaging user experience by:
  - preventing any event to be fired (we want cancel events to undo the disabled
    state)
  - removing the element from keyboard focus flow (bad accessibility)
  - hardcoding CSS

  We semantically mark an element as aria-disabled, while still
  allowing some events and keyboard navigation."
  #?(:cljs (remove (fn [^js event] (.. event -target (closest "[aria-disabled=true]"))))))

(defn gen-event-handlers "The rule is ::ui/<dom event name>-event adds a dom event listener."
  [cancel-sym props transducers {:keys [ignore-aria-disabled]}]
  (map (fn [signal]
         (let [callback       (get props signal)
               [ack callback] (if (vector? callback) callback [nil callback])
               xf             (let [xf (get transducers signal)]
                                (cond ignore-aria-disabled xf
                                      xf                   `(comp xf-ignore-aria-disabled ~xf)
                                      :else                `xf-ignore-aria-disabled))
               event          (if (= ::keychord-event signal)
                                `(dom/>keychord-events ~(::keychords props) ~xf)
                                `(dom/>events ~(signal->event signal) ~xf))]
           (if (some? ack)
             `[~signal (impulse ~ack ~callback ~event)]
             `[~signal (auto-impulse ~cancel-sym ~callback ~event)])))
    (signals props)))

(defn parse-props
  ([valuef props transducers]
   (parse-props valuef props transducers nil))
  ([valuef props transducers cancel-sym]
   (parse-props valuef props transducers cancel-sym {}))
  ([valuef props transducers cancel-sym opts]
   (assert (map? props))
   [(valuef props nil)
    (gen-event-handlers cancel-sym props transducers opts)
    (let [dom-props (data/select-ns :hyperfiddle.photon-dom props)]
      (if-let [type (::type props)]
        (assoc dom-props :type type)
        dom-props))]))

(defn ^:no-doc swap!* [!atom f & _]
  (swap! !atom f))

(def events (comp dom/mappend dom/bubble dom/mappend))

#?(:clj
   (defn element* [tag props pending-props events pending-events cancel-impulse-sym & body]
     `(dom/bubble
        (~tag (dom/props ~props)
         (let [!cancel#            (atom false)
               ~cancel-impulse-sym (p/watch !cancel#)]
           (try (into [(do ~@(dom/handle-text body))] [~@events])
                (catch Pending t
                  (dom/props ~pending-props)
                  (handle #{::cancel} (partial swap!* !cancel# not) (events [~@pending-events]))
                  (throw t))))))))

(defmacro element [tag props & body]
  (let [cancel-impulse-sym               (gensym "cancel-impulse_")
        [_ pending-events pending-props] (parse-props (constantly nil) (::pending props {}) {} cancel-impulse-sym
                                           {:ignore-aria-disabled true})
        [_ events props]                 (parse-props (constantly nil) props {} cancel-impulse-sym)]
    (apply element* tag props pending-props events pending-events cancel-impulse-sym body)))

(defmacro button [props & body]
  `(element dom/button ~props ~@body))

(defmacro checkbox
  ([] `(checkbox {}))
  ([props]
   (let [cancel-impulse-sym               (gensym "cancel-impulse_")
         [_ pending-events pending-props] (parse-props (constantly nil) (::pending props {}) {} cancel-impulse-sym
                                            {:ignore-aria-disabled true})
         [value events props]                 (parse-props ::value props {} cancel-impulse-sym)]
     (apply element* `dom/input
       (merge {::dom/type          :checkbox
               ::dom/checked       value
               ::dom/indeterminate `(nil? ~value)}
         props)
       pending-props
       events
       pending-events
       cancel-impulse-sym
       nil))))

(defn format-num [format-str x] #?(:cljs (if format-str
                                           (format format-str x)
                                           (pr-str x))))
(defn parse-num [x] #?(:cljs (-> (js/parseFloat x) (* 100) (js/Math.round) (/ 100))))
(defn is-num? [x] #?(:cljs (not (js/isNaN x))))
(defn numeric-value [x] (let [num (parse-num x)] (when (is-num? num) num)))
(def parse-input (comp (map (dom/oget :target :value)) (map parse-num) (filter is-num?)))

(defmacro numeric-input [props]
  (let [[value events props'] (parse-props ::value props {})]
    `(dom/bubble
       (dom/input (dom/props ~props')
         (dom/props {:value (format-num ~(::format props) ~value)
                     :type  :number})
         (into [[::value (dom/events "input" parse-input ~value)]]
           [~@events])))))

(defmacro input
  ([] `(input {}))
  ([props & body]
   (case (::type props)
     :number   `(numeric-input ~props)
     :checkbox `(checkbox ~props)
     (let [cancel-impulse-sym               (gensym "cancel-impulse_")
           [_ pending-events pending-props] (parse-props (constantly nil) (::pending props {}) {} cancel-impulse-sym
                                              {:ignore-aria-disabled true})
           [value events props]             (parse-props ::value props {} cancel-impulse-sym)]
       (apply element* `dom/input
         (assoc props ::dom/value value)
         pending-props
         (into `[[::value (dom/events "input" (map (dom/oget :target :value)) ~value)]]
           events)
         pending-events
         cancel-impulse-sym
         body)))))

(defmacro textarea
  ([] `(textarea {}))
  ([props & body]
   (let [cancel-impulse-sym               (gensym "cancel-impulse_")
         [_ pending-events pending-props] (parse-props (constantly nil) (::pending props {}) {} cancel-impulse-sym
                                                       {:ignore-aria-disabled true})
         [value events props]             (parse-props ::value props {} cancel-impulse-sym)]
     (apply element* `dom/textarea
       (assoc props ::dom/value value)
       pending-props
       (into `[[::value (dom/events "input" (map (dom/oget :target :value)) ~value)]] events)
       pending-events
       cancel-impulse-sym
       body))))

(defn- index-of [vec val] (.indexOf vec val))

(defn parse-select-value [options]
  (fn [event]
    (get options (parse-num (dom/oget event :target :value)))))

;; (defn parse-select-value [options] (comp (map (dom/oget :target :value)) (map parse-num) (map (partial get options))))

(defmacro select [props]
  (let [auto-options          (gensym "options_")
        [value events props'] (parse-props ::value props {::change-event `(map (juxt identity (parse-select-value ~auto-options)))})
        options               (::options props)
        auto-value            (gensym "value_")]
    `(let [~auto-options (vec ~options)
           ~auto-value   ~value
           selected#     (index-of ~auto-options ~auto-value) ; TODO accept a keyfn prop instead of comparing with `=`.
           ]
       (dom/bubble
         (dom/select (dom/props ~props')
           (dom/props {:value ~auto-value})
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
        auto-list             (str (gensym "list_"))]
    `(dom/bubble
       (let [res#     (dom/input (dom/props ~props')
                        (dom/props {:type  :search
                                    :list  ~auto-list
                                    :value ~value})
                        (into [[::value (dom/events "input" (map (dom/oget :target :value)) ~value)]]
                          [~@events]))
             needle#  (::value res#)
             options# (when-let [options# ~(::options props `(p/fn [_#] nil))]
                        (new options# needle#))]
         (dom/datalist {:id ~auto-list}
           (p/for [option# options#]
             (dom/option {:id (:value option#)} (dom/text (:text option#)))))
         (vec res#)))))

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
