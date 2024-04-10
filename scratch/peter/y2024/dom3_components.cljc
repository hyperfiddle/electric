(ns peter.y2024.dom3-components
  (:require [hyperfiddle.electric-de :as e :refer [$]]
            [hyperfiddle.electric-dom3 :as dom]))

;; goal - converge (dom/div (dom/button)) and ($ Div ($ Button)) styles
;; experiment - thunk components, mount in 1 go

(comment
  ;; this is problematic in current design, although the requirement seems simple:
  ;; create a new DOM component derived from an existing one, adding some props
  (e/defn DivContainer []
    (dom/div
      (dom/props {:class "container"})
      'bodyhere))
  ;; how do we add body?
  (defmacro div-container [& body]
    `(dom/div (dom/props {:class "container"}) ~@body))
  ;; here we had to use a macro
  ;; what if we want to inject this component into another one?
  ;; Not possible as a macro
  )


(e/defn Nobody [])

;; a DOM component => a 1-arg e/fn taking a Body continuation, returning a 0-arg e/fn
(e/defn Div [Body] (e/fn [] (dom/with (.createElement js/document "div") ($ Body))))

;; ugly but works
(binding [dom/node js/document.body]
  ($ ($ Div
       (e/fn []
         ($ ($ Div
              (e/fn []
                (dom/style '...)
                ($ ($ Div Nobody)))))
         ($ ($ Div Nobody))))))

;; remove the boilerplate
(defmacro $$ [Compo & body] `($ ($ ~Compo (e/fn [] ~@body))))

(binding [dom/node js/document.body]
  ($$ Div
    ($$ Div
      (dom/style '...)
      ($$ Div))
    ($$ Div)))

;; initial problem was to e.g. create div container with props and pass DOM nodes in

(e/defn DivContainer [Body]
  (e/fn []
    (dom/with (.createElement js/document "div")
      (dom/props {:class "container"})
      ($ Body))))

;; or
(e/defn DivContainer [Body]
  (e/fn []
    ($$ Div
      (dom/props {:class "container"})
      ($ Body))))

(binding [dom/node js/document.body]
  ($$ DivContainer
    ($$ Div (dom/props '..))))

;; higher order component
(e/defn MyComponentBuilder [ChildComponent]
  (e/fn [Body]
    (e/fn [] ($$ ChildComponent) ($ Body))))

(binding [dom/node js/document.body]
  ($$ Div
    ;; ugly, we have to `$` and then `$$`
    ($$ ($ MyComponentBuilder Div))))

;; possible helper to write higher order components
(defmacro component [body-sym & body] `(e/fn [~body-sym] (e/fn [] ~@body)))

(e/defn MyCompoBuilder [ChildComponent]
  (component Body
    ($$ ChildComponent)
    ($ Body)))

;; how would we write the TxButton3 example from event handling

(e/defn TxButton3 [BusyBody Tx]
  (component Body
    (dom/button
      (let [!evt (atom nil), !busy (atom nil), busy (boolean (e/watch !busy))]
        (dom/listen "click" (partial reset! !evt))
        (dom/props {:disabled busy})
        (when (e/watch !evt)
          ;; contract - Tx calls `done` once done, returns busy state
          (reset! !busy ($ Tx #(reset! !evt nil))))
        ($ BusyBody busy))
      ($ Body))))

;; usage of above
(e/defn ButtonUsage []
  ($$ ($ TxButton3
        (e/fn [busy] (dom/style {:aria-busy busy, :background-color (when busy "yellow")}))
        (e/fn [done] (case (e/input (transact! :conn (inc (get-count db))))
                       ::pending true #_else (done))))))

;; TxButton3 shows syntax trickery, ideally we'd like to flatten the e/fns
(e/defn TxButtonFlat [BusyBody Tx Body]
  ($$ dom/Button
    (let [!evt (atom nil), !busy (atom nil), busy (boolean (e/watch !busy))]
      (dom/listen "click" (partial reset! !evt))
      (dom/props {:disabled busy})
      (when (e/watch !evt)
        ;; contract - Tx calls `done` once done, returns busy state
        (reset! !busy ($ Tx #(reset! !evt nil))))
      ($ BusyBody busy))
    ($ Body)))

;; now there's a syntactic clash on `$$`
;; - on one hand we want to thunk the body continuation to remove the (e/fn []) boilerplate
;; - on the other hand we want to pass an unknown number of positional arguments before

(comment
  ;; this doesn't work, but we'd like it to work
  ($$ TxButtonFlat
    ;; these are positional
    (e/fn [busy] (dom/style {:aria-busy busy, :background-color (when busy "yellow")}))
    (e/fn [done] (case (e/input (transact! :conn (inc (get-count db))))
                   ::pending true #_else (done)))
    ;; this is thunked body
    (dom/props 'txbuttonpropshere)
    (dom/style 'stylehere))

  ;; we could have a number of positional macros
  ;; $$0 $$1 $$2 ...
  ;; quite ugly and error prone
  ($$2 TxButtonFlat '..)


  ;; we could take the positionals in a vector form
  ($$ Div
    ($$ TxButtonFlat [(e/fn [busy]) (e/fn [done])]
      'bodyhere))
  ;; or
  ($$ Div
    ($$ [TxButtonFlat (e/fn [busy]) (e/fn [done])]
      'bodyhere))

  ;; calling by hand is still ugly, right?
  ($ ($ Div
       (e/fn []
         ($ ($ TxButtonFlat
              (e/fn [busy])
              (e/fn [done])
              (e/fn [] '...)))))))
