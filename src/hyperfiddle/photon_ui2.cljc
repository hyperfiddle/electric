(ns hyperfiddle.photon-ui2
  (:refer-clojure :exclude [long double keyword symbol uuid])
  (:require
   clojure.edn
   [clojure.string :as str]
   [contrib.str :refer [blank->nil]]
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros hyperfiddle.photon-ui2)))

(p/defn Focused? []
  (p/with-cycle [focused false]
    (if focused
      (nil? (dom/Event. "blur" false))
      (some? (dom/Event. "focus" false)))))

(defn- ?static-props [body] (if (map? (first body)) `((dom/props ~(first body)) ~@body) body))

(p/defn InputController [controlled-value]
  (p/with-cycle [value ""]
    (if (Focused?.)
      (if-some [e (dom/Event. "input" false)]
        (-> e .-target .-value) value)
      (set! (.-value dom/node) controlled-value))))

(defmacro input "
A dom input text component.
Purpose of this component is to eventually sync the input with the database which is also an input
of this component.

When component doesn't have focus, input value and return value reflect argument.
When component has focus, argument is ignored and return value reflects user input.

TODO: what if component loses focus, but the user input is not yet committed ?
" [controlled-value & body]
  `(dom/with
     (dom/dom-element dom/node "input")
     (.setAttribute dom/node "type" "text")
     ~@(?static-props body)
     (new InputController ~controlled-value)))

(p/defn DemoInput []
  (dom/h1 "a controlled input that reverts on blur")
  (let [a (input "hello world")]
    (dom/pre a)) ; latest value

  (dom/h1 "a controlled input with looped state")
  (let [a (p/with-cycle [a "hello world"]
            (input a))]
    (dom/pre a)))

(p/defn Button [label busy] ; todo props and task
  (dom/with (dom/dom-element dom/node "button")
    (dom/set-text-content! dom/node label)
    (dom/Event. "click" busy)))

(comment
  (when-some [click-event (Button. "click me" false)] ; see event for one frame
    (println click-event))) ; nil -> active event -> nil

(defmacro textarea [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "textarea")
     ~@(?static-props body)
     (new InputController ~controlled-value)))

(p/defn ^:private -Edn-editor [x] ; optimize macroexpansion size
  (when-some [x (blank->nil x)]
    (try (clojure.edn/read-string x)
         (catch :default _ nil))))

(defmacro edn-editor [x & body]
  `(new -Edn-editor (textarea (pr-str ~x) ~@body))) ; optimize static body props

(p/defn InputValues [controlled-value focused< input< on-blur]
  (p/with-cycle [v nil]
    (if (new focused<) (if-some [in (new input<)] in v) (do (on-blur controlled-value) controlled-value))))

(p/defn ->ParsedEvent [typ busy parser]
  (p/fn [] (when-some [e (dom/Event. typ busy)] (parser e))))

(defmacro checkbox [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "input")
     (dom/props {:type "checkbox"})
     ~@(?static-props body)
     (new InputValues ~controlled-value Focused? (new ->ParsedEvent "change" false #(.. % -target -checked))
       #(set! (.-checked dom/node) %))))

(p/defn ValueOn [event-type]
  (p/with-cycle [v (.-value dom/node)]
    (if-let [ev (dom/Event. event-type false)] (.. ev -target -value) v)))

(defmacro select [options controlled-value & body]
  `(let [opts# ~options, cv# ~controlled-value]
     (dom/with (dom/dom-element dom/node "select")
       (?static-props ~@body)
       (p/for [opt# opts#] (dom/option (dom/props (dissoc opt# :text)) (some-> opt# :text dom/text)))
       (set! (.-value dom/node) cv#)
       (new ValueOn "change"))))

(p/defn Value []
  (p/with-cycle [v (.-value dom/node)]
    (if-let [ev (dom/Event. "input" false)] (-> ev .-target .-value) v)))

;; TODO clicking the arrows on the input doesn't trigger new values flowing out
;; the reason is the input doesn't get focused by clicking them
(defmacro long [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "input")
     (dom/props {:type "number"})
     ~@(?static-props body)
     (new InputValues ~controlled-value Focused? (new ->ParsedEvent "input" false #(-> % .-target .-value parse-long))
       #(set! (.-value dom/node) %))))

(defmacro double [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "input")
     (dom/props {:type "number"})
     ~@(?static-props body)
     (new InputValues ~controlled-value Focused? (new ->ParsedEvent "input" false #(-> % .-target .-value parse-double))
       #(set! (.-value dom/node) %))))

(defn parse-edn [s] (try (clojure.edn/read-string s) (catch #?(:clj Throwable :cljs :default) _)))
(defn keep-if [pred v] (when (pred v) v))
(defn parse-keyword [s] (keep-if keyword? (parse-edn s)))
(defn parse-symbol [s] (keep-if symbol? (parse-edn s)))

(defmacro keyword [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "input")
     (dom/props {:type "text"})
     ~@(?static-props body)
     (new InputValues ~controlled-value Focused? (new ->ParsedEvent "input" false #(-> % .-target .-value parse-keyword))
       #(set! (.-value dom/node) %))))

(defmacro symbol [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "input")
     (dom/props {:type "text"})
     ~@(?static-props body)
     (new InputValues ~controlled-value Focused? (new ->ParsedEvent "input" false #(-> % .-target .-value parse-symbol))
       #(set! (.-value dom/node) %))))

(defmacro uuid [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "input")
     (dom/props {:type "text" :pattern "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"})
     ~@(?static-props body)
     (new InputValues ~controlled-value Focused? (new ->ParsedEvent "input" false #(-> % .-target .-value parse-uuid))
       #(set! (.-value dom/node) %))))

;; TODO codemirror?
(defmacro edn [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "textarea")
     (dom/props {:type "text"})
     ~@(?static-props body)
     (new InputValues ~controlled-value Focused? (new ->ParsedEvent "input" false #(-> % .-target .-value parse-edn))
       #(set! (.-value dom/node) %))))

(defn parse-date [s]
  #?(:clj (java.time.LocalDate/parse s)
     :cljs (-> s js/Date.parse js/Date. .toISOString (str/replace #"T.*$" ""))))

;; TODO what type of value should we accept and what should we return?
;; currently `parse-date` for cljs returns a short string representation
;; of the date in format yy-mm-dd. We expect the same format as input
(defmacro date [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "input")
     (dom/props {:type "date"})
     ~@(?static-props body)
     (new InputValues ~controlled-value Focused? (new ->ParsedEvent "input" false #(-> % .-target .-value parse-date))
       #(set! (.-value dom/node) %))))
