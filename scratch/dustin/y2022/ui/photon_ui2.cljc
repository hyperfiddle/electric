(ns hyperfiddle.photon-ui2
  (:refer-clojure :exclude [long double keyword symbol uuid])
  (:require
   clojure.edn
   [clojure.string :as str]
   [contrib.str :refer [blank->nil pprint-str]]
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.rcf :as rcf :refer [tests tap with %]])
  #?(:cljs (:require-macros hyperfiddle.photon-ui2))
  (:import (hyperfiddle.photon Pending)))

(p/defn Focused? []
  (p/with-cycle [focused false]
    (if focused
      (nil? (dom/Event. "blur" false))
      (some? (dom/Event. "focus" false)))))

(defn- ?static-props [body] (if (map? (first body)) `((dom/props ~(first body)) ~@body) body))

(defn nil->pending [value]
  ; workaround - use nil as sentinel to mean inital value is unknown.
  ; We must never emit a manufactured value, because the database would commit it!
  (if (nil? value) (throw (Pending.)) value))

(p/defn InputController [controlled-value]
  ; When cv is pending, the exception is thrown in two places, see RCF.
  ; This tells us that the cv is not yet synced.
  ; We use nil as a sentinel value that indicates the value is unknown.
  (nil->pending
    (p/with-cycle [local-value #_(throw (Pending.)) nil] ; internal state starts unknown
      (if (Focused?.)
        (if-some [e (dom/Event. "input" false)]
          (-> e .-target .-value) local-value)
        ; on fast blur, keep local value until controlled value commits.
        ; (Prevent leapfrog loop – blur too fast -> get old controlled value, loop)
        (try (when (p/Unglitch. controlled-value) (set! (.-value dom/node) controlled-value))
             (catch Pending _ local-value)))))) ; emit local value (that matches the dom) while we wait for commit

; Note: if the body of with-cycle throws, the state is not updated (currently).

(tests
  "the same exception is thrown from two places!"
  (p/defn InputController1 [tap controlled-value]
    (try controlled-value
         (catch Pending _ (tap :pending-inner))))

  (with (p/run (try
                 (InputController1. tap (throw (Pending.)))
                 (catch Pending _ (tap :pending-outer)))))
  % := :pending-inner
  % := :pending-outer
  % := ::rcf/timeout)

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
; the input is stable because at some point the user stops typing
; What prevents the infinite loop is at some point the state is stable, no events
; update the state and due to work skipping nothing happens.

(p/defn DemoInput []
  (dom/h1 "a controlled input that reverts on blur")
  (let [a (input "hello world")]
    (dom/pre a)) ; latest value

  (dom/h1 "a controlled input with looped state")
  (let [a (p/with-cycle [a "hello world"]
            (input a))]
    (dom/pre a)))

(p/defn ^:deprecated Button [label busy] ; todo props and task
  (dom/with (dom/dom-element dom/node "button")
    (dom/set-text-content! dom/node label)
    (dom/Event. "click" busy)))

(comment
  (when-some [click-event (Button. "click me" false)] ; see event for one frame
    (println click-event))) ; nil -> active event -> nil

(def IDLE false)
(def BUSY true)

(defmacro button
  "buttons manage a 'task' (async photon thunk). See wip.demo-ui2 for examples"
  [props & body]
  `(dom/element
     :button
     ~props ; static, should we compile out props not in ::dom namespace?
     ; how to compile unqualified props to current namespace? ;~(auto-props props)
     (do (p/with-cycle [busy# IDLE]
           (dom/set-property! dom/node "aria-busy" busy#)
           (dom/set-property! dom/node "disabled" busy#) ; ?
           (try
             (when-some [e# (dom/Event. "click" busy#)]
               (new ~(::click-event props) e#))
             IDLE (catch Pending _ BUSY)))
         nil) ; defend against leaking with-cycle state
     ~@body)) ; likely a label

(comment
  (p/defn F [e] (p/server (d/transact! conn [(tx-from-db db)])))

  (button {::click-event F}
    "toggle client/server 4")

  (when-some [e (button "toggle client/server 4")]
    (F. e))

  (p/with-cycle [busy# IDLE]
    (try
      (when-some [e (button "toggle client/server 4" busy#)]
        (F. e))
      IDLE (catch Pending _ BUSY)))


  (button {::click-event (p/fn [e] (p/server (swap! !x not)))}
    "toggle client/server 4")

  (when-some [e (button "toggle client/server 4")]
    (p/server (swap! !x not)))

  ; see user.demo-2-toggle for demos

  ; check props were compiled
  (macroexpand-1
    '(button {:click-event (p/fn [e] (p/server (swap! !x not)))}
       "toggle client/server 4")))

; experiment
(defmacro button2 "buttons manage a 'task' (async photon thunk)"
  [busy props & body]
  `(dom/element
     :button
     ~props ; static, should we compile out props not in ::dom namespace?
     ; how to compile unqualified props to current namespace? ;~(auto-props props)
     (let [busy# ~busy]
       (dom/set-property! dom/node "aria-busy" busy#) ; auto-set?
       #_(dom/set-property! dom/node "disabled" busy#) ; this can be set through props now
       ~@body ; ?
       (dom/Event. "click" busy#))))

(defmacro textarea [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "textarea")
     ~@(?static-props body)
     (new InputController ~controlled-value)))

#?(:cljs (defn read-str-maybe [x] ; optimize macroexpansion size
           (when-some [x (blank->nil x)]
             (try (clojure.edn/read-string x)
                  (catch :default _ nil)))))

(defmacro edn-editor [x & body]
  `(read-str-maybe (textarea (pprint-str ~x) ~@body))) ; optimize static body props

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

(defmacro select [options controlled-value & body]
  `(let [opts# (vec ~options),
         cv#   ~controlled-value]
     (p/with-cycle [current# cv#]
       (dom/with (dom/dom-element dom/node "select")
         (?static-props ~@body)
         (p/for [[idx# opt#] (map-indexed vector opts#)]
           (dom/option
             (dom/props (-> opt# (dissoc :text) (assoc :value idx#)))
             (when (= (:value opt#) current#)
               (dom/props {:selected true}))
             (some-> opt# :text dom/text)))
         (if (empty? opts#)
           current#
           (if-some [event (dom/Event. "change" false)]
             (:value (get opts# (js/parseInt (.. event -target -value))))
             current#))))))

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

(defn parse-edn [s] (try (some-> s contrib.str/blank->nil clojure.edn/read-string) (catch #?(:clj Throwable :cljs :default) _)))
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
  (try
    #?(:clj (java.time.LocalDate/parse s)
       :cljs (js/Date. s))
    (catch #?(:clj Throwable :cljs :default) _)))

(defn parse-datetime-local [s]
  (try
    #?(:clj (java.time.LocalDateTime/parse s)
       :cljs (js/Date. s))
    (catch #?(:clj Throwable :cljs :default) _)))

;; TODO what type of value should we accept and what should we return?
;; currently `parse-date` for cljs returns a short string representation
;; of the date in format yyyy-mm-dd. We expect the same format as input
(defmacro date [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "input")
     (dom/props {:type "date"})
     ~@(?static-props body)
     (new InputValues ~controlled-value Focused? (new ->ParsedEvent "input" false #(-> % .-target .-value parse-date))
       #(set! (.-value dom/node) %))))
