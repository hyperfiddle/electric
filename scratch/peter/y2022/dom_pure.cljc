(ns peter.y2022.dom-pure
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.rcf :refer [tap tests with]])
  #?(:cljs (:require-macros peter.y2022.dom-pure)))

(p/def node)

(defmacro mount [elem _props & body]
  `(let [elem# ~elem] (.appendChild node elem#) (binding [node elem#] ~@body)))

(defn elem [typ] #?(:cljs (.createElement js/document typ)))
(defmacro mounted-elem [typ opts & body] `(mount (elem ~typ) ~opts ~@body))
(defmacro dl [opts & body] `(mounted-elem "dl" ~opts ~@body))
(defmacro dt [opts & body] `(mounted-elem "input" ~opts ~@body))
(defmacro dd [opts & body] `(mounted-elem "dd" ~opts ~@body))

(p/def event)
(declare handle)

(defn ->f [c] (-> c (* 9) (/ 5) (+ 32)))
(defn ->c [f] (-> f (- 32) (* 5) (/ 9)))
(p/defn TemperatureConverter []
  (p/client
    (binding [node (.getElementById js/document "root")]
      (let [c-input (elem "input")
            f-input (elem "input")
            [temperature _pending] (p/init 0 (p/union (handle c-input "oninput" (-> event :target :value js/parseFloat))
                                               (handle f-input "oninput" (-> event :target :value js/parseFloat ->c))))]
        (dl
          (dt "Celsius")    (dd (mount c-input {:value temperature}))
          (dt "Fahrenheit") (dd (mount f-input {:value (->f temperature)})))))))

(tests
  ;; let's say we want to count # of clicks of a button on the server
  (def counter #?(:clj (atom 0)))
  (with (p/run (let [btn (elem "button")
                     [_click pending] (tap (p/init 0 (handle btn "onclick" (p/server (swap! counter inc)))))]
                 (mount btn {:aria-busy pending})
                 (.click btn)))
    ;; `handle` returns a continuous flow (the body is photon code)
    ;; without an initial value (there's no initial click event)
    ;; p/init provides the initial value out-of-band
    ;; since the photon body might hop over wire it can turn to pending state
    ;; p/init therefore joins 2 continuous flows
    ;; - 1 for the last processed value
    ;; - 1 true/false whether a new value is currently pending (being processed)
    ;;
    ;; initially we have a value right away
    % := [0 false]
    ;; after the click we are waiting for the server hop so we have no new value yet
    % := [0 true]
    ;; once the server returns we stop pending and have a new last processed value
    % := [1 false]
    )
  )
