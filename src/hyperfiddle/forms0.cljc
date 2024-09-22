(ns hyperfiddle.forms0
  #?(:cljs (:require-macros hyperfiddle.forms0))
  (:require [contrib.str :refer [pprint-str]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.input-zoo0 :refer [Button!]]))

; commit/discard with staging area
; inputs - emit as you type - with a token
; stage - monitor edits and accumulate them
; button - batch all edits into single token, chained with upstream tokens

(e/defn Field* [e a Control]
  (e/for [[t v] (Control)]
    [t [[::update e a v]]]))

(defmacro Field [e a body] `(Field* ~e ~a (e/fn [] ~body)))

(defn merge-cmds [& txs] (vec (apply concat txs))) ; datomic style

(e/defn Stage "implies an explicit txn monoid" ; doesn't work on raw values
  ([edits] (Stage edits :merge-cmds merge-cmds))
  ([[ts txs :as edits] ; amb destructure
    & {:keys [debug merge-cmds]
       :or {merge-cmds merge-cmds
            debug false}}]
   (e/client
     (let [batch (apply merge-cmds (e/as-vec txs))
           reset (let [ts (e/as-vec ts)]
                   (fn token
                     ([] (doseq [t ts] (t)))
                     #_([err] (doseq [t ts] (t err ::keep))))) ; we could route errors to dirty fields, but it clears dirty state
           n (e/Count edits)
           [us _ :as btns] (e/amb ; todo progress
                               (Button! ::commit :disabled (zero? n) :label "commit")
                               (Button! ::discard :disabled (zero? n) :label "discard")
                               (e/When debug (dom/span (dom/text " " n " dirty"))))]
       (e/amb
         (e/for [[u cmd] btns]
           (case cmd ; does order of burning matter?
             ::discard (case ((fn [] (us) (reset))) ; clear any in-flight commit yet outstanding
                         (e/amb)) ; clear edits, controlled form will reset
             ::commit [(fn token
                         ([] (reset) (u))
                         ([err] (u err) #_(reset err))) ; leave dirty fields dirty, activates retry button
                       batch])) ; commit as atomic batch

         (e/When debug
           (dom/pre (dom/props {:style {:min-height "4em"}})
             (dom/text (pprint-str batch :margin 80)))))))))