(ns wip.snake
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [missionary.core :as m])
  #?(:cljs (:require-macros wip.snake)))

(def initial-state
  {:paused?      false
   :position     [6 6]
   :history      ()
   :width        12
   :height       12
   :pill-density 0.02
   :pills        #{}                                        ;[4 4] [5 8] ;; generate?
   :velocity     [-1 0]                                     ;; random?
   :size         3
   :dead?        false
   :interval     250})

(defonce !state (atom initial-state))
(def !tick-interval (cursor !state [:interval]))
(def !paused? (cursor !state [:paused?]))
(def !dead? (cursor !state [:dead?]))

(def cell-colors
  {:head "gold"
   :worm "green"
   :pill "red"
   :wall "grey"})

(p/defn Cell [x y type]
  (dom/div
    (dom/style
      {::dom/float         "left"
       ::dom/clear         (if (zero? x) "left")
       ::dom/width         "24px"
       ::dom/height        "24px"
       ::dom/background    (get cell-colors type "#eee")
       ::dom/border-radius "0.5em"
       ::dom/margin        "1px"
       ::dom/border-right  "1px solid #777"
       ::dom/border-bottom "1px solid #777"})))

(defn handle-keys! [!state event]                           ;; ideally this should emit an event, not mutate state directly
  (let [key (.-keyCode event)]
    (case key
      32 (swap! !paused? not)                               ; spacebar
      13 (if @!dead? (reset! !state initial-state))         ; enter
      (when-let [dir (case key
                       37 :left
                       39 :right
                       nil)]
        (.preventDefault event)
        (swap! !state assoc :direction dir))))
  nil)

(defn next-state [{:keys [position size velocity direction pills pill-density width height history] :as state}]
  (let [snake (set (take size history))
        found-pill? (pills position)
        [dx dy] velocity
        new-vel (case direction                             ; 90-degree rotation matrices
                  :left [dy (- dx)]
                  :right [(- dy) dx]
                  [dx dy])
        new-pos (mapv mod (map + new-vel position) [width height])
        pills (if found-pill? (disj pills position) pills)
        missing-pills (- (* pill-density width height) (count pills))
        new-pills (take missing-pills (repeatedly #(vector (rand-int width) (rand-int height))))]
    (assoc state
      :dead? (if (snake new-pos) true false)
      :direction nil
      :velocity new-vel
      :history (take size (conj history position))
      :position new-pos
      :interval (- 250 (* 3 size))
      :pills (into pills new-pills)
      :size (if found-pill? (inc size) size))))


(p/defn World [{:as state :keys [pills size width height history position]}]
  (let [worm (set (take size history))]
    (dom/div
      (p/for [y (range height)]
        (p/for [x (range width)]
          (let [pos [x y]
                cell-type (cond
                            (= position pos) :head
                            (worm pos) :worm
                            (pills pos) :pill)]
            (Cell. x y cell-type)))))))

(p/defn Game [state]
  #_(js/window.addEventListener "keydown" (partial handle-keys! state))
  #_(if (and (not @!dead?) (not @!paused?))
    (swap! !state next-state))
  (World. state)
  #_(js/window.removeEventListener "keydown" key-handler))

(p/defn App []
  (dom/h1 (dom/text "Multiplayer Snake"))
  #_(dom/div (dom/p (dom/button (dom/text "Tick!"))))
  (Game. (p/watch !state)))
