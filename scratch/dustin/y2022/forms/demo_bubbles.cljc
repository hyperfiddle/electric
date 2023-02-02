(ns wip.demo-bubbles
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [contrib.str :refer [pprint-str]])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros wip.demo-bubbles)))

(defn map-commands [f xs] (into [] (comp (map f) (remove nil?)) xs))

(defn command->statement [[tag value]] (case tag
                                         :set-checked value
                                         nil))

(p/defn App []
  (p/client
    (dom/div
      (dom/h1 (dom/text "Statement bubbling"))
      (dom/p (dom/text "Photon-dom elements collect and return statements."))
      (let [commands (dom/div
                       [:command "a command is a pair"] ; todo ack
                       (let [{:keys [::ui/value]} (ui/input {::ui/value "initial"})]
                         [:set-text value])
                       [[:command 1] [:command 2]] ; they can be grouped
                       (dom/ul
                         [:location :nested-in-ul] ; they bubble up from children dom nodes
                         (p/for [x [1 2 3]]
                           (dom/li (dom/label
                                     [:set-checked {:checkbox/id x
                                                    :checkbox/value (::ui/value (ui/checkbox))}]
                                     (dom/text " " x))))))]
        (dom/p (dom/text "Commands:"))
        (dom/pre
          (dom/text (pprint-str commands)))
        (dom/p (dom/text "Statements:"))
        (dom/pre
          (dom/text (pprint-str (map-commands command->statement commands))))))))

(def main
  #?(:cljs (p/boot
             (try
               (binding [dom/node (dom/by-id "root")]
                 (App.))
               (catch Pending _)))))

(comment
  #?(:clj (user/browser-main! `main))
  )

; call-cc lets us return commands upstack to an interpreter
; and when run, they resume and throw pending from a local context
; so that the busy state for each command is handled locally to each command
; letting us easily have a separate busy state per command without coordination
; which lets us use the stage pattern like in TodoMVC with many kinds of commands
; not just staging area commands


; Todomvc has the reframe pattern
; Can we rewrite TodoMVC without component local side effects (and state)?
; G: Yes, we had that, but we had issues with loops, distributed glitch, no pending state colocation
; (don't know where to show the spinner).
; todomvc checkboxes are programmed as buttons, that was a mistake

; database/stage commands
; route commands
; compound commands (change tab and focus)

; today in Rosie, edits are directives and routes are side effect.
; We'll need to unify them, which means either all directive or all side effects
; busy? if all directives, only have page level loading state, never localized button state
; if all side effects (checkbox button or route hyperlink) - local busy state opportunity followed
; by page loading state in response to the effect. This is discrete programming and impure
; Side effects are local; this is problematic in the reframe compound transition case.
; if you want reframe, you cannot have local side effects. This means you lose local busy state.
; Or, algebraic effects with resume, now you have BOTH local busy state AND centrally injected handlers

(comment
  ; https://gist.github.com/dustingetz/2c1916766be8a61baa39f9f88feafc44
  ; https://www.eff-lang.org/try/
  (handle
    (ui/button {::dom/class "destroy"
                ::ui/click-event (p/fn [_]
                                   (p/with-cycle [busy false]
                                     (dom/set-property! dom/node "aria-busy" busy)
                                     (dom/set-property! dom/node "disabled" busy)
                                     (perform [::transact! [[:db/retractEntity id]]]))) ; perform is throw?
                ::ui/pending {::dom/aria-busy true}})
    (with ::transact! tx k ->
      ; note Photon already almost has resume for Pending, though with constraints. Doable!
       (let [busy (try
                    (p/server (d/transact! conn tx))
                    false (catch Pending _ true))]
         (continue k busy)))
    (with Exception e
      (throw e)))

  (handle
    (p/with-cycle [busy false]
      (dom/set-property! dom/node "aria-busy" busy)
      (dom/set-property! dom/node "disabled" busy)
      (try
        (perform [::transact! [[:db/retractEntity id]]])
        false (catch Pending _ true)))
    (with ::transact! tx k ->
          (p/server (d/transact! conn tx))
          (continue k)))
  )

; If you don't care about throwing local loading states, you can just use this pattern today
; with command bubbling, no need for algebraic effects. This is bubbling pattern that we already have.

(comment
  (reframe-async dom/node)

  (reframe '(set-busy dom/node)
           '(p/server (transact! [[:db/retractEntity id]])))

  (try
    (p/call-cc p/frame #(p/server (transact! [[:db/retractEntity id]])))
    (catch ...)))

; G: Can we emulate reframe directives with local busy state given m/cp?
; m/cp makes a global registry cheap to watch
; global registry of busy states, now it requires coordination, call-cc solves this