(ns peter.y2022.dom-pure
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
   [missionary.core :as m]
   [hyperfiddle.photon-dom :as dom])
  (:import [missionary Cancelled]
           [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros peter.y2022.dom-pure)))

(defn event* [nod typ] #?(:cljs (m/observe (fn [!] (.addEventListener nod typ !) #(.removeEventListener nod typ !)))))
(defmacro event [typ] `(new event* dom/node ~typ))

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

(p/def it)
(defmacro d2c [init-val & pairs]
  `(let [init-val# ~init-val
         [branch# v#] (new (m/relieve {} (m/reductions {} [::init] (indexed-flows ~@(take-nth 2 pairs)))))]
     (binding [it v#]
       (case branch# ~@(interleave (range) (take-nth 2 (rest pairs))) init-val#))))

(defmacro flow-cond* [& pairs]
  (let [init-val (last pairs)
        pairs    (drop-last 2 pairs)]
    `(let [init-val#    ~init-val
           [branch# v#] (new (m/relieve {} (m/reductions {} [::init] (indexed-flows ~@(take-nth 2 pairs)))))]
       (binding [it v#]
         (try
           (case branch# ~@(interleave (range) (take-nth 2 (rest pairs))) init-val#)
           (catch Pending _ ::pending))))))

(defn- reduce-to-pairs [flow]
  (m/reductions (fn [[lpv _pending] nx] (case nx ::pending [lpv true] [nx false])) [nil false] flow))

(defmacro flow-cond [& pairs] `(new (reduce-to-pairs (flow-cond* ~@pairs))))

(defn pending-pair [flow] (m/reductions (fn [[lpv _] nx] (case nx ::pending [lpv true] [nx false])) [nil false] flow))
(defmacro reify-pending [& body] `(-> (p/fn [] (try (do ~@body) (catch Pending _ ::pending))) pending-pair new))

(defmacro flow-case [init-val & clauses]
  `(let [init-val# ~init-val
         [branch# v#] (new (m/relieve {} (m/reductions {} [::init] (indexed-flows ~@(take-nth 2 clauses)))))]
     (binding [it v#]
       (case branch# ~@(interleave (range) (take-nth 2 (rest clauses))) init-val#))))

(p/def prev)
(defmacro flow-case2 [init-val & clauses]
  `(let [init-val# ~init-val
         prev# (atom init-val#)
         [branch# v#] (new (m/relieve {} (m/reductions {} [::init] (indexed-flows ~@(take-nth 2 clauses)))))]
     (binding [it v#]
       (binding [prev @prev#]
         (reset! prev# (case branch# ~@(interleave (range) (take-nth 2 (rest clauses))) init-val#))))))

(comment
  (p/defn Focused? []
    (p/with-cycle [focused false]
      (case focused
        true (not (some? (dom/Event. "blur" false)))
        false (some? (dom/Event. "focus" false)))))
  ;; vs
  (p/defn Focused? []
    (flow-case false                    ; no cycle
      (event "focus") true              ; no some? or other checks
      (event "blur")  false))

  (p/with-cycle [input-value controlled-value]
    (or (some-> (dom/Event. "input" false) ; never busy - process synchronously
          .-target .-value)                ; set new local value
      input-value))
  ;; vs
  (let [input-value (flow-case controlled-value
                      (event "input") (-> it .-target .-value))])

  (defn interval [ms] (m/ap (m/? (m/?> (m/seed (repeat (m/sleep ms ms)))))))
  (time (m/? (m/reduce #(prn %2) nil (m/eduction (take 5) (interval 500)))))

  (def discard (p/run (tap (flow-case 0 (interval 1000) it (interval 700) it))))
  (discard)

  (p/defn Button [label busy]
    (dom/with (dom/dom-element dom/node "button")
      (dom/set-text-content! dom/node label)
      (dom/Event. "click" busy)))
  ;; vs
  ;; this I don't consider idiomatic as it leaks the events
  (p/defn Button [label]                ; no busy leaking
    (dom/with (dom/dom-element dom/node "button")
      (dom/set-text-content! dom/node label)
      (flow-case nil (event "click") it)))
  ;; this is more idiomatic
  (p/defn Button [label init-val Handler]
    (dom/with (dom/dom-element dom/node "button")
      (dom/set-text-content! dom/node label)
      (flow-case init-val (event "click") (Handler. it))))
  ;; this is best
  (defmacro button [label & body]
    `(new (dom/with (dom/dom-element dom/node "button")
            (dom/set-text-content! dom/node ~label)
            ~@body)))
  ;; then at call site
  (button "click me"
    (flow-case 0 (event "click") (inc prev)))
  ;; don't couple object with behavior OOP style
  ;; just provide fns to mix the behavior in
  ;; e.g. button that prints which mouse button was clicked
  (button "click me"
    (flow-case nil
      (event "contextmenu") (.preventDefault it)
      (event "mousedown")   (println (case (.button it) 0 "Left" 1 "Middle" 2 "Right") "button clicked")))
  ;; vs
  ;; can't reuse Button, we need other events
  (dom/with (dom/dom-element dom/node "button")
    (dom/set-text-content! dom/node "click me")
    (some-> (Event. "contextmenu" false) .preventDefault)
    (when-some [e (Event. "mousedown" false)]
      (println (case (.button e) 0 "Left" 1 "Middle" 2 "Right") "button clicked")))

  ;; example: button that gets another css class on hover
  (Button. "hover me"
    (flow-case nil
      (event "mouseenter") (-> it .-target .-classList (.add "surprise"))
      (event "mouseleave") (-> it .-target .-classList (.remove "surprise"))))
  ;; vs.
  (dom/with (dom/dom-element dom/node "button")
    (dom/set-text-content! dom/node "hover me")
    (when-some [e (Event. "mouseenter" false)] (-> e .-target .-classList (.add "surprise")))
    (when-some [e (Event. "mouseleave" false)] (-> e .-target .-classList (.remove "surprise"))))
  ;; note how the event handling machinery leaks
  ;; one has to understand busy
  ;; one has to remember to wrap event with when-some
  ;; flow-case is n-1, it rids us of busy impulse machinery completely
  )

(tests
  (def mbx (m/mbx))
  (def flow (m/ap (loop [] (m/amb (m/? mbx) (recur)))))
  (def discard (p/run (try (tap (d2c ::start flow (inc it)))
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

  (p/def it)
  (declare handle)

  (defn ->f [c] (-> c (* 9) (/ 5) (+ 32)))
  (defn ->c [f] (-> f (- 32) (* 5) (/ 9)))
  (p/defn TemperatureConverter []
    (p/client
      (binding [node (.getElementById js/document "root")]
        (let [c-input (elem "input")
              f-input (elem "input")
              [temperature _pending] (p/init 0 (p/union (handle c-input "oninput" (-> it :target :value js/parseFloat))
                                                 (handle f-input "oninput" (-> it :target :value js/parseFloat ->c))))]
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
    (d->c 0 (event* btn "onclick") (p/server (swap! counter inc)))
    (let [[branch v] (new (m/relieve {} (m/reductions {} ::init (event* btn "onclick"))))]
      (if (= ::init v)
        0
        (binding [it v]
          (case branch
            0 (p/server (swap! counter inc))
            1 ...))))
    )

  (tests
    (with (p/run
            (try
              (tap (d->c 100
                     (emit-delayed 50 (range 3)) (let [e it] (p/server e))))
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

(defn puppet-flow []
  (let [mbx (m/mbx)
        flow (m/ap (loop [] (m/amb= (m/? mbx) (recur))))]
    [flow mbx]))
(tests
  (do
    (def a-puppet (puppet-flow))
    (def a-flow (first a-puppet))
    (def feed (second a-puppet)))
  (def cancel ((m/reduce #(tap %2) nil (m/cp (or (m/? (m/sleep 300 :hi)) :foobar))) identity identity))
  (def cancel ((m/reduce #(prn %2) nil (m/cp (m/?< (m/reductions {} :hi a-flow)))) identity identity))
  (def cancel ((m/reduce #(prn %2) nil (m/reductions {} :hi a-flow)) identity identity))
  (feed 2)
  (cancel)

  (m/? (m/reduce #(prn %2) nil (p/task->cp (m/sp (m/? (m/sleep 1000 :done))))))

  (def latency-ms 300)
  (defn reset-slowly! [atm v] (p/task->cp (m/sp (m/? (m/sleep latency-ms)) (reset! atm v))))
  (def !x (atom 0))
  (m/? (m/reduce #(prn %2) nil (reset-slowly! !x 1)))

  )
