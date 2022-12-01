(ns hyperfiddle.photon-ui2
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

(defmacro select [options & body]
  `(let [opts# ~options]
     (dom/with (dom/dom-element dom/node "select")
       (?static-props ~@body)
       (p/for [opt# opts#] (dom/option (dom/props (dissoc opt# :text)) (some-> opt# :text dom/text)))
       (p/with-cycle [uv# (.-value dom/node)]
         (if-let [ev# (dom/Event. "change" false)] (-> ev# .-target .-value) uv#)))))

;; TODO ui/input puts the resulting value into value attribute of the DOM input element
(p/defn Value []
  (p/with-cycle [v (.-value dom/node)]
    (if-let [ev (dom/Event. "input" false)] (-> ev .-target .-value) v)))
