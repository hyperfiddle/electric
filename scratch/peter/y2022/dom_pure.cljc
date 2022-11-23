(ns peter.y2022.dom-pure
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
   [missionary.core :as m])
  (:import [missionary Cancelled]
           [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros peter.y2022.dom-pure)))

(defn events [nod typ] #?(:cljs (m/observe (fn [!] (.addEventListener nod typ !) #(.removeEventListener nod typ !)))))

(defmacro indexed-flows [& flows]
  (let [n (count flows)]
    ;; thunked to force clj compilation in photon
    `((fn [] (m/ap (case (m/?> ~n (m/seed (range ~n)))
                     ~@(mapcat (fn [[i flow]] `(~i [~i (m/?> ~flow)]))
                         (map vector (range) flows))))))))

(tests
  ((m/reduce #(tap %2) nil (indexed-flows (m/seed [:a]) (m/seed [:b]))) identity identity)
  % := [1 :b]
  % := [0 :a])

(p/def event)
(defmacro d2c [init-val & pairs]
  `(let [init-val# ~init-val
         [branch# v#] (new (m/relieve {} (m/reductions {} [::init] (indexed-flows ~@(take-nth 2 pairs)))))]
     (binding [event v#]
       (case branch# ~@(interleave (range) (take-nth 2 (rest pairs))) init-val#))))

(tests
  (def mbx (m/mbx))
  (def flow (m/ap (loop [] (m/amb (m/? mbx) (recur)))))
  (def discard (p/run (try (tap (d2c ::start flow (inc event)))
                           (catch missionary.Cancelled _)
                           (catch hyperfiddle.photon.Pending _)
                           (catch Throwable e (prn e)))))
  % := ::start
  (mbx 1)
  % := 2
  (mbx 2)
  % := 3
  (discard)
  )

(comment
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

  #_
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

  ;; discrete to continuous
  ;; take an initial value
  ;; and discrete flow + photon code pairs
  ;; return a continuous flow that has the initial value
  ;; and unites the discrete flows into a single continuous flow
  ;; where each discrete event is handled by the accompanied photon code
  ;; Joins a tuple of reactive values [last-processed-value pending]
  (comment


    (def ^:dynamic *v*)
    (defmacro clj-d->c [init-val & pairs]
      `(m/cp (let [init-val# ~init-val
                   [branch# v#] (m/?< (m/relieve {} (m/reductions {} [::init] (indexed-flows ~@(take-nth 2 pairs)))))]
               (binding [*v* v#]
                 (case branch# ~@(interleave (range) (take-nth 2 (rest pairs))) init-val#)))))

    (defn emit-delayed [sleep vs] (m/ap (m/? (m/sleep sleep (m/?> (m/seed vs))))))

    (m/? (m/reduce conj (clj-d->c 100
                          (emit-delayed 5 (range 3)) (doto *v* prn))))

    (= #{[0 :a] [1 :b]} (m/? (m/reduce conj #{} (indexed-flows (m/seed [:a]) (m/seed [:b])))))
    (d->c 0 (events btn "onclick") (p/server (swap! counter inc)))
    (let [[branch v] (new (m/relieve {} (m/reductions {} ::init (events btn "onclick"))))]
      (if (= ::init v)
        0
        (binding [event v]
          (case branch
            0 (p/server (swap! counter inc))
            1 ...))))
    )

  (tests
    (with (p/run
            (try
              (tap (d->c 100
                     (emit-delayed 50 (range 3)) (let [e event] (p/server e))))
              (catch missionary.Cancelled _)
              (catch hyperfiddle.photon.Pending _ (tap :pending))
              (catch Throwable e (println e))))
      % := 100
      % := :pending
      % := 0
      % := 1
      % := 2
      )

    )

  (tests
    (with (p/run (tap (new (m/reductions {} nil (emit-delayed 5 (range 3))))))
      % := nil
      % := 0
      % := 1
      % := 2
      ))
  )
