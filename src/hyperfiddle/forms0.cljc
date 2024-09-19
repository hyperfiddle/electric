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

(e/defn Stage "implies an explicit txn monoid"
  ([edits] (Stage merge-cmds edits))
  ([merge-cmds [ts txs :as edits]] ; amb destructure
   (let [batch (apply merge-cmds (e/as-vec txs))
         reset (let [ts (e/as-vec ts)] #(doseq [t ts] (t)))
         n (e/Count edits)
         !commit-err (atom nil) commit-err (e/watch !commit-err)
         [ts _ :as btns] (e/amb
                           (Button! ::commit :label "commit" :disabled (zero? n) :error commit-err) ; todo progress
                           (Button! ::discard :label "discard" :disabled (zero? n))
                           (dom/span (dom/text " " n " dirty")))]
     (e/amb
       (e/for [[t cmd] btns]
         (case cmd
           ::discard (case (ts (reset)) ; clear any in-flight commit yet outstanding
                       (e/amb)) ; clear edits, controlled form will reset
           ::commit [(fn token
                       ([] (t (reset)))
                       ([err] (reset! !commit-err err) (t)))
                     batch])) ; commit as atomic batch

       (dom/pre (dom/props {:style {:min-height "4em"}})
         (dom/text (pprint-str batch :margin 80)))))))