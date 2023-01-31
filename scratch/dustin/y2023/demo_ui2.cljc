(ns wip.demo-ui2
  (:require
    [contrib.str :refer [pprint-str]]
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom :as dom]
    [hyperfiddle.photon-ui2 :as ui])
  #?(:cljs (:require-macros wip.demo-ui2))
  (:import (hyperfiddle.photon Pending)))

(defn reset!-ret-nil [& args] (apply reset! args) nil)

(p/defn App []
  (p/client

    (dom/h1 "controlled input strategies")
    (dom/p "Requirement: the input must be above the rendered value. Currently controls must
     hoist state and loop back. Is this desired?")

    (dom/dl
      (dom/dt "ui/input (reset!)")
      (dom/dd (let [!x (atom "a") x (p/watch !x)]
                (->> (ui/input x)
                     (reset! !x)) ; value leak
                nil)) ; gross

      (dom/dt "ui/input (reset! with field underneath)")
      (dom/dd (let [!x (atom "a") x (p/watch !x)]
                (->> (ui/input x)
                     (reset! !x)) ; concealed value leak
                (dom/pre (pr-str x)))) ; leaky value is shadowed, but not solved

      (dom/dt "ui/input (reset!2 which returns nil)")
      (dom/dd (let [!x (atom "a") x (p/watch !x)]
                (->> (ui/input x)
                     (reset!-ret-nil !x)))) ; cute - value is handled, return nil

      #_#_
      (dom/dt "ui/input (reset! with field underneath, no leak)")
      (dom/dd (let [!x (atom "a") x (p/watch !x)]
                (ui/input-ret-nil x {::ui/on-value (p/fn [x] (reset! !x x))}))) ; no leak

      (dom/dt "ui/input (cycle)")
      (dom/dd (do (p/with-cycle [x "a"]
                    (ui/input x))
                  nil)) ; gross (L: agree)
      ;; L: the problem is, dom/dd tries to interpret the result of body

      (dom/dt "ui/input (cycle with field underneath)")
      (dom/dd (do (p/with-cycle [x "a"]
                    (let [x' (ui/input x)]
                      (dom/pre (pr-str x))
                      x')) ; contorted
                  nil)) ; gross

      (dom/dt "ui/input (cycle with field underneath)")
      (dom/dd (do (p/with-cycle [x "a"]
                    (doto (ui/input x)
                      (-> pr-str dom/pre))) ; contorted too
                  nil)) ; gross too

      #_#_
      (dom/dt "ui/input (change with-cycle to return nil?)")
      (dom/dd
        (p/with-cycle-ret-nil [x "a"]
          (ui/input x))) ; local, never leaks

      #_#_
      (dom/dt "ui/input (change with-cycle to return nil?)")
      (dom/dd (p/with-cycle-ret-nil [x "a"]
                (doto (ui/input x)
                  (println)))) ; opportunity to send it up in discrete time

      #_#_
      (dom/dt "ui/input (named recursion)")
      (dom/dd (new (p/fn Rec [x]
                     (Rec. (ui/input x)) ; incorrect, too many frames
                     nil) ; legal, gross
                   "a"))

      #_#_
      (dom/dt "ui/input (named recursion)")
      (dom/dd (new (p/fn Rec [x]
                     (Rec. (ui/input x)) ; incorrect, too many frames
                     (dom/pre (pr-str x))) ; legal
                   "a"))

      #_#_
      (dom/dt "ui/input (recur)")
      (dom/dd (loop [x "a"]
                (recur (ui/input x))
                nil)) ; gross and illegal

      #_#_
      (dom/dt "ui/input (recur)")
      (dom/dd (loop [x "a"]
                (recur (ui/input x)) ;
                (dom/pre (pr-str x)))) ; illegal

      #_#_
      (dom/dt "ui/input (recur)")
      (dom/dd (let [x (loop [x "a"]
                        (recur (ui/input x)))]
                (dom/pre (pr-str x))))


      )

    (dom/h1 "controlled form, no distribution")
    (dom/p "todo: behavior under distribution - stage pattern")
    (dom/p "todo: behavior under distribution - impulse pattern (?)")

    (let [!x (atom {:input1 "hello"
                    :select1 "a"
                    :long1 42
                    :double1 1.123
                    :kw1 :foo
                    :sym1 'foo
                    :uuid1 #uuid "00000000-0000-0000-0000-000000000000"
                    :edn1 {:hello "world"}
                    :date1 "2022-11-30" ; todo wrong type
                    })
          x (p/watch !x)]

      (dom/dl
        (dom/dt "ui/input")
        (dom/dd (->> (ui/input (:input1 x))
                     (swap! !x assoc :input1))
                (ui/input (:input1 x) {::dom/disabled true})) ; todo disabled input should emit nil

        (dom/dt "ui/select")
        (dom/dd (->> (ui/select [{:text ""} {:text "a"} {:text "b"}] (:select1 x))
                     (swap! !x assoc :select1)))

        (dom/dt "ui/long")
        (dom/dd (->> (ui/long (:long1 x))
                     (swap! !x assoc :long1)))

        (dom/dt "ui/double")
        (dom/dd (->> (ui/double (:double1 x))
                     (swap! !x assoc :double1)))

        (dom/dt "ui/keyword")
        (dom/dd (->> (ui/keyword (:kw1 x))
                     (swap! !x assoc :kw1)))

        (dom/dt "ui/symbol")
        (dom/dd (->> (ui/symbol (:sym1 x))
                     (swap! !x assoc :sym1)))

        (dom/dt "ui/uuid")
        (dom/dd (->> (ui/uuid (:uuid1 x))
                     (swap! !x assoc :uuid1)))

        (dom/dt "ui/edn")
        (dom/dd (->> (ui/edn (:edn1 x))
                     (swap! !x assoc :edn1)))

        (dom/dt "ui/date")
        (dom/dd (->> (ui/date (:date1 x))
                     (swap! !x assoc :date1))))

      (dom/pre (pprint-str x)))

    #_
    (p/with-cycle [x {:input1 "hello"
                      :select1 "a"
                      :long1 42
                      :double1 1.123
                      :kw1 :foo
                      :sym1 'foo
                      :uuid1 #uuid "00000000-0000-0000-0000-000000000000"
                      :edn1 {:hello "world"}
                      :date1 "2022-11-30" ; todo wrong type
                      }]

      (dom/dl
        (merge                                              ;; dustin finds it gross (L: it's not)
          (dom/dt "ui/input")                               ;; nil is a valid map for merge
          (dom/dd {:input1 (ui/input (:input1 x))})         ;; maps are not interpreted as text (luckily)

          (dom/dt "ui/select")
          (dom/dd {:select1 (ui/select [{:text ""} {:text "a"} {:text "b"}] (:select1 x))})

          (dom/dt "ui/long")
          (dom/dd
            (merge
              {:long1 (ui/long (:long1 x))}
              (dom/dt "ui/long")
              (dom/dd {:long1 (ui/long (:long1 x))})))

          (dom/dt "ui/long")
          (dom/dd {:long1 (ui/long (:long1 x))})

          (dom/dt "ui/date")
          (dom/dd {:date1 (ui/date (:date1 x))})))

      (dom/pre (pprint-str x)))



    (dom/h1 "buttons manage tasks")
    (dom/p "this button manages a 'task' (photon thunk), managing the busy state
    so the task cannot be clicked while outstanding. Todo: task buttons should present
    affordances for cancellation, success, failure, retry and progress.")
    (p/server
      (let [!x (atom true) x (p/watch !x)]
        (p/client
          (dom/dl
            (dom/dt "ui/button (click-event)")
            (dom/dd
              (ui/button {::ui/click-event (p/fn [e]
                                             (p/server (swap! !x not)))}
                "toggle client/server")
              (dom/span " " (if (p/server x)
                              (p/client (pr-str (type 1)))
                              (p/server (pr-str (type 1))))))

            (dom/dt "ui/button2 (byo pending management)")
            (dom/dd
              (let [!busy (atom false) busy (p/watch !busy)]
                (->> (try
                       (when-some [click-event (ui/button2 busy {} (dom/text "toggle client/server"))]
                         (p/server (swap! !x not))) ; pending
                       false (catch Pending _ true))
                     (reset! !busy))
                nil)
              (dom/span " " (if (p/server x)
                              (p/client (pr-str (type 1)))
                              (p/server (pr-str (type 1))))))
            ))))

    (dom/h2 "uncontrolled inputs")
    (dom/p "Do we want to support controls that latch their own state?")

    (dom/h2 "field states")
    (dom/p "todo: blur, disabled, etc")
    (dom/p "todo: static props, dynamic props")))
