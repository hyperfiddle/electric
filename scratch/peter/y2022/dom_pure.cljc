(ns peter.y2022.dom-pure
  (:require
   [hyperfiddle.photon :as p]
   [missionary.core :as m])
  #?(:cljs (:require-macros peter.y2022.dom-pure)))

(p/def node)

(defn listen [elem event]
  #?(:cljs
     (m/observe (fn [!] (.addEventListener elem event !) #(.removeEventListener elem event !)))))

(defmacro mount! [elem & body]
  `(let [elem# ~elem] (.appendChild node elem#) (binding [node elem#] ~@body)))

(defn elem [typ _opts] #?(:cljs (.createElement js/document typ)))
(defmacro mounted-elem [typ opts & body] `(mount! (elem ~typ ~opts) ~@body))
(defmacro dl [opts & body] `(mounted-elem "dl" ~opts ~@body))
(defmacro dt [opts & body] `(mounted-elem "input" ~opts ~@body))
(defmacro dd [opts & body] `(mounted-elem "dd" ~opts ~@body))

(defn on [elem event-type f] (m/ap (f (m/?> (listen elem event-type)))))
(defmacro amb= [& flows] `(m/ap (m/amb= ~@flows)))
(defn amb2 [f1 f2] (m/ap (m/amb= f1 f2)))

(defn ->f [c] (-> c (* 9) (/ 5) (+ 32)))
(defn ->c [f] (-> f (- 32) (* 5) (/ 9)))
(p/defn TemperatureConverter []
  (p/client
    (binding [node (.getElementById js/document "root")]
      (let [c-input (elem "input" {:step 0.5})
            f-input (elem "input" {:step 0.5})
            ;; I'd want to write something along the lines of
            ;; [temperature busy] (p/join-with 0 (p/amb= (on c-input "oninput" (-> event :target :value js/parseFloat))
            ;;                                     (on c-input "oninput" (-> event :target :value js/parseFloat ->c))))
            ;; where the `on` body is photon and `event` is a p/def
            temperature (new (m/reductions {} 0 (amb2
                                                  (on c-input "oninput" #(-> % :target :value js/parseFloat))
                                                  (on f-input "oninput" #(-> % :target :value js/parseFloat ->c)))))]
        (.setAttribute c-input "value" temperature)
        (.setAttribute f-input "value" (->f temperature))
        (dl
          (dt "Celsius")    (dd (mount! c-input))
          (dt "Fahrenheit") (dd (mount! f-input)))
        temperature))))
