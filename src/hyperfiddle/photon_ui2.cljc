(ns hyperfiddle.photon-ui2
  (:refer-clojure :exclude [long double keyword])
  (:require
   clojure.edn
   [contrib.str :refer [blank->nil]]
   [missionary.core :as m]
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.rcf :as rcf :refer [tests tap with %]])
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

(defmacro checkbox [controlled-value & body]
  `(dom/with (dom/dom-element dom/node "input")
     (.setAttribute dom/node "type" "checkbox")
     ~@(?static-props body)
     (let [cv# ~controlled-value]
       (if (new Focused?)
         (p/with-cycle [uv# cv#] (if-some [ev# (dom/Event. "change" false)] (.. ev# -target -checked) uv#))
         (set! (.-checked dom/node) cv#)))))

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
  `(let [cv# ~controlled-value]
     (dom/with (dom/dom-element dom/node "input")
       (.setAttribute dom/node "type" "number")
       ~@(?static-props body)
       (p/with-cycle [v# nil]
         (if (new Focused?)
           (if-some [e# (dom/Event. "input" false)]
             (if-some [vv# (-> e# .-target .-value parse-long)] vv# v#)
             v#)
           (do (set! (.-value dom/node) (pr-str cv#)) cv#))))))

(comment
  ;; possible decomposition where the domain logic can be tested separately
  ;; this p/fn can be tested on clj as well, no dom
  (p/defn long-values [controlled-value focused< input< on-blur]
    (p/with-cycle [v nil] (if (new focused<) (or (new input<) v) (do (on-blur) controlled-value))))

  (defmacro long* [controlled-value & body]
    `(let [cv# ~controlled-value]
       (dom/with (dom/dom-element dom/node "input")
         (.setAttribute dom/node "type" "number")
         ~@(?static-props body)
         (new long-values cv# Focused?
           (p/fn [] (when-some [e (dom/Event. "input" false)] (-> e .-target .-value parse-long)))
           #(set! (.-value dom/node) (pr-str cv#)))))))

;; TODO should we take a fomatter, if yes should we also take a parser?
(defmacro double [controlled-value formatter & body]
  `(let [cv# ~controlled-value, formatter# ~formatter]
     (dom/with (dom/dom-element dom/node "input")
       (.setAttribute dom/node "type" "number")
       ~@(?static-props body)
       (p/with-cycle [v# nil]
         (if (new Focused?)
           (if-some [e# (dom/Event. "input" false)]
             (if-some [vv# (-> e# .-target .-value parse-double)] vv# v#)
             v#)
           (do (set! (.-value dom/node) (formatter# cv#)) cv#))))))

(defn parse-keyword [s]
  (try (let [parsed (clojure.edn/read-string s)]
         (when (keyword? parsed) parsed))
       (catch #?(:clj Throwable :cljs :default) _)))

(defmacro keyword [controlled-value & body]
  `(let [cv# ~controlled-value]
     (dom/with (dom/dom-element dom/node "input")
       (.setAttribute dom/node "type" "text")
       ~@(?static-props body)
       (p/with-cycle [v# nil]
         (if (new Focused?)
           (if-some [e# (dom/Event. "input" false)]
             (if-some [vv# (-> e# .-target .-value parse-keyword)] vv# v#)
             v#)
           (set! (.-value dom/node) cv#))))))
