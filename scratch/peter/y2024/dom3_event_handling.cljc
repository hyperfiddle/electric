(ns peter.y2024.dom3-event-handling
  (:require [hyperfiddle.electric-de :as e :refer [$]]
            [hyperfiddle.electric-dom3 :as dom]
            [missionary.core :as m]
            [hyperfiddle.incseq :as i]))

;;;;;;;;;;;;;;
;;; EVENTS ;;;
;;;;;;;;;;;;;;

;; (dom/listen "input" #(-> % .-target .-value))
;; starts as the empty incseq, afterwards has a single changing value
;; (), ("f"), ("fo"), ("foo") ...
;; to be implemented, right now it starts as nil
;; (nil), ("f"), ("fo"), ("foo") ...

;; (dom/event-log "keydown" #(when (= "enter" (.-key %)) [% (-> % .-target .-value)]))
;; returns a log (append-only incseq) of values derived from the event stream
;; {}, {1 "buy milk"}, {1 "buy milk", 2 "visit grandma"}, ...

#?(:cljs
   ;; TODO starts as empty incseq, later singleton changing value
   (defn listen1 [nd typ f opts]
     (m/observe (fn [!]
                  (! nil)
                  (let [! (comp ! f) , opts (clj->js opts)]
                    (.addEventListener nd typ ! opts)) #(.removeEventListener nd typ ! opts)))))

(defmacro listen
  ([typ] `(listen ~typ identity))
  ([typ f] `(listen dom/node ~typ ~f))
  ([nd typ f] `(listen ~nd ~typ ~f nil))
  ([nd typ f opts] `(e/input (listen1 ~nd ~typ ~f ~opts))))

(defn append-only [<xs]
  (->> <xs
    (m/eduction (map-indexed (fn [i x] {:grow 1 :degree (inc i) :shrink 0 :permutation {} :change {i x} :freeze #{i}})))
    (m/reductions {} (i/empty-diff 0))
    (m/relieve i/combine)))

#?(:cljs
   (defn event-log* [nd typ f opts]
     (append-only
       (m/observe (fn [!]
                    (let [! #(when-some [v (f %)] (! v)) , opts (clj->js opts)]
                      (.addEventListener nd typ ! opts)) #(.removeEventListener nd typ ! opts))))))

(defmacro event-log
  ([typ] `(event-log ~typ identity))
  ([typ f] `(event-log dom/node ~typ ~f))
  ([nd typ f] `(event-log ~nd ~typ ~f nil))
  ([nd typ f opts] `(e/input (event-log* ~nd ~typ ~f ~opts))))

;;;;;;;;;;;;;;;;;;;
;;; INPUT FIELD ;;;
;;;;;;;;;;;;;;;;;;;

;; input field matches electric semantics

(e/defn InputField []
  ;; this abstraction is useless, electric semantics match this perfectly
  (dom/input
    (listen "input" #(-> % .-target .-value))))

;;;;;;;;;;;;;;;;;
;;; TX BUTTON ;;;
;;;;;;;;;;;;;;;;;

;; requirements:
;; - run on click
;; - disable while transacting
;; - prevent cycles
;; - allow userland to read busy state

(defn transact! [conn v] (m/ap (m/amb= ::pending (m/? (m/sleep 1000 ::done)))))
(declare db get-count)

(e/defn TxButton [Body Tx]
  (dom/button
    (let [!evt (atom nil), !busy (atom nil), busy (boolean (e/watch !busy))]
      (dom/listen "click" (partial reset! !evt))
      (dom/props {:disabled busy})
      (when (e/watch !evt)
        ;; contract - Tx returns `::pending` (only) while Tx is running
        (reset! !busy (case ($ Tx (e/watch !evt))
                        ::pending true
                        #_else (reset! !evt nil))))
      ($ Body busy))))

;; usage of above
(e/defn ButtonUsage []
  ($ TxButton
    (e/fn [busy] (dom/style {:aria-busy busy, :background-color (when busy "yellow")}))
    (e/fn [_evt] (e/input (transact! :conn (inc (get-count db)))))))

;; different abstraction, different contract
(e/defn TxButton2 [Body Tx]
  (dom/button
    (let [!evt (atom nil), !busy (atom nil), busy (boolean (e/watch !busy))]
      (dom/listen "click" (partial reset! !evt))
      (dom/props {:disabled busy})
      (when (e/watch !evt)
        ;; contract - Tx sets !evt to nil once done, returns busy state
        (reset! !busy ($ Tx !evt)))
      ($ Body busy))))

;; usage of above
(e/defn ButtonUsage2 []
  ($ TxButton2
    (e/fn [busy] (dom/style {:aria-busy busy, :background-color (when busy "yellow")}))
    (e/fn [!evt] (case (e/input (transact! :conn (inc (get-count db))))
                   ::pending true #_else (reset! !evt nil)))))

;; another abstraction
(e/defn TxButton3 [Body Tx]
  (dom/button
    (let [!evt (atom nil), !busy (atom nil), busy (boolean (e/watch !busy))]
      (dom/listen "click" (partial reset! !evt))
      (dom/props {:disabled busy})
      (when (e/watch !evt)
        ;; contract - Tx calls `done` once done, returns busy state
        (reset! !busy ($ Tx #(reset! !evt nil))))
      ($ Body busy))))

;; usage of above
(e/defn ButtonUsage3 []
  ($ TxButton3
    (e/fn [busy] (dom/style {:aria-busy busy, :background-color (when busy "yellow")}))
    (e/fn [done] (case (e/input (transact! :conn (inc (get-count db))))
                   ::pending true #_else (done)))))

;;;;;;;;;;;;;;;;;;
;;; CREATE NEW ;;;
;;;;;;;;;;;;;;;;;;

;; create new matches e/cursor semantics

#?(:cljs (defn value-on-enter [nd evt] (when (= "enter" (.-key evt)) [evt (.-value nd)])))

(e/defn CreateNew []
  (let [in (dom/input (dom/props {:placeholder "what needs to be done?"}) dom/node)]
    (dom/ul
      (e/cursor [v (event-log in "keydown" (partial value-on-enter in))]
        (dom/li (dom/text v))))))

;; OPTIMISTIC UPDATES - merge 2 incseqs (is there an operator for that?)
;; `value-on-enter` would need to change to return an ID key that will persist

(declare query-todos)
(e/defn CreateNewOptimistic []
  (let [in (dom/input (dom/props {:placeholder "what needs to be done?"}) dom/node)]
    (dom/ul
      (e/cursor [v (e/merge (event-log in "keydown" (partial value-on-enter in)) (query-todos db))]
        (dom/li (dom/text v))))))
