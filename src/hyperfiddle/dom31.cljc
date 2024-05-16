;; * DONE Replace dom3 by dom3_efn
;;   G: diffed both files, LGTM
;; * DONE move event handling to separate ns
;;   So we can think clearly
;;   We can always merge back later
;; * DONE Implement dom/text
;; * DONE Implement dom/comment
;; * TODO Implement dom/div
;; * TODO Implement dom/div nesting
;; * TODO Implement setting attributes
;; * TODO Implement setting class
;; * TODO Implement setting inline style
;; * TODO Implement event handling

(ns hyperfiddle.dom31
  (:refer-clojure :exclude [comment])
  (:require
   [hyperfiddle.electric-de :as e :refer [$]]
   ;; [contrib.assert :as ca]
   [missionary.core :as m]
   ;; [hyperfiddle.electric.impl.lang-de2 :as lang]
   ;; #?(:cljs [goog.dom])
   ))

;;;;;;;;;;;;;;;
;; Reference ;;
;;;;;;;;;;;;;;;

;; e/tag                                 ;; electric clojure only. resolves to an invariant+singleton unique identifier.
;; (e/mount-point)                       ;; clojure function returning a fresh container associating tags to stateful items which can be observed as an incseq with e/join. The ordering of items reflects the ordering of tags in the program order.

;; (e/insert! mount-point tag init)      ;; add a new item associated with `tag` in `mount-point`, with initial state `init`.
;; (e/update! mount-point tag f & args)  ;; change state of item associated with `tag` in `mount-point` by applying function `f` to current state, with optional following arguments `args`.
;; (e/remove! mount-point tag)           ;; remove item associated with `tag` in `mount-point`.


;;;;;;;;;;;;;
;; General ;;
;;;;;;;;;;;;;

(def node)

#?(:cljs
   (let [key (js/Symbol.for "hyperfiddle.mount-point")]
     (defn mount-point
       ([node] (aget node key))
       ([node v] (aset node key v)))))

#?(:cljs
   (defn attach! [parent-node tag e]
     (assert (instance? js/Node parent-node))
     (m/observe (fn [!]
                  (! nil)
                  (let [mount-point (mount-point node)] ; TODO could this be inlined?
                    (e/insert! mount-point tag e)
                    #(e/remove! mount-point tag e))))))

;;;;;;;;;;
;; Text ;;
;;;;;;;;;;

(e/defn Text [str] ; ^::lang/print-clj-source
  (e/client
    (let [e (.createTextNode js/document "")]
      (e/input (attach! node (e/tag) e))
      (set! (.-textContent e) str))))

(defmacro text [& strs] `(do ~@(for [s strs] `($ Text ~s))))

;;;;;;;;;;;;;
;; Comment ;;
;;;;;;;;;;;;;

(e/defn Comment [str] ; ^::lang/print-clj-source
  (e/client
    (let [e (.createComment js/document "")]
      (e/input (attach! node (e/tag) e))
      (set! (.-textContent e) str))))

(defmacro comment [& strs] `(do ~@(for [s strs] `($ Comment ~s))))


;;;;;;;;;;;;;
;; Element ;;
;;;;;;;;;;;;;

;; TODO Understand this with 100% precision.
;; G: here is my understanding of it (as comments), to be verified.
;; Leo says its done
(defn mount-items
  [element ; a dom element to mount children in

                       ; an incseq's diff
   {:keys [grow        ; number of added items
           shrink      ; number of removed items
           degree      ; max size of collection (after grow, before shrink)
           permutation ; map of indexes movements e.g. "a replaced by b"
           change      ; map of index -> value
           ]}]
  (let [children    (.-childNodes element)  ; current children of the element to mount in
        move        (i/inverse permutation) ; permutation is "a replaced by b" move is "b replaces a"
        size-before (- degree grow)         ; current expected child list size
        size-after  (- degree shrink)       ; expected child list size after patch
        ]
    ;; Step 1 - Additions and replacements by new elements
    (loop [i      size-before ; start with expected current size and scan left to right, one by one,
                                        ; until diff's degree (before removals)
           change change]
      (if (not (== i degree)) ; If there are items to add
        ;; Addition
        (let [j (get move i i)]                 ; get the index where this item is or went
          (.appendChild element (get change j)) ; add children to the list
          (recur (inc i) (dissoc change j)))    ; continue with next change.
        ;; If there are changes but no items to add, then its just in-place changes
        (run! (fn [[i element']] ; for each change
                (.replaceChild element element'   ; replace old child at index i by element' (new child)
                  (.item children (get move i i)) ; get old child index
                  ))
          change)))
    ;; Step 2 - Removals and reorders of existing elements
    (loop [permutation permutation
           i           degree]
      (if (not (== i size-after)) ; if there are extra items
        ;; Removals
        (let [i (dec i) ; move to the penultimate item index
              j (get permutation i i)] ; get the place where the item went (it might have been moved left)
          (.removeChild element (.item children j)) ; remove child there (causing a shift to the left)
          (recur (i/compose permutation (i/rotation i j)) i)) ; continue with next extra item, accounting for the left shift
        ;; Nothing to remove, but children to reorder
        (loop [permutation permutation]
          (when-not (= permutation {}) ; if there are still some swaps to apply
            (let [[i j] (first permutation)] ; we will apply the first swap in the list
              (.insertBefore element (.item children j) ; insert at target position
                (.item children                         ; the child from origin position
                  (if (< j i) (inc i) i))) ; due to insertBefore, if j is left of i, insert to the right of the target position.
              (recur (i/compose permutation (i/rotation i j))) ; continue with the rotation applied,
                                                               ; which cancels out remaining swaps
                                                               ; appropriately.
              )))))
    element))

(e/defn Element [tag Body]
  (e/client
    (let [e   (.createElement js/document (name tag))
          mp  (e/mount-point)
          tag (e/tag)]
      (e/input (attach! node tag e)) ; mount and unmount element in parent
      (e/join (mount-items e mp))    ; interprets diffs to mount and maintain children in correct order
      (mount-point e mp)             ; expose mount point to children
      (binding [node e]              ; run continuation
        (Body.)))))

(defmacro element [tag & body]
  `($ Element ~tag (e/fn [] (e/amb ~@body))))

(clojure.core/comment "comment var is already taken")

;; #?(:cljs (defn node? [v] (instance? js/Node v)))
;;
;; #?(:cljs (defn appending> [elem parent]
;;            (ca/is parent node? "DOM node parent is not an HTML Node. Maybe dom/node is unbound?" {:parent parent})
;;            (m/observe (fn [!] (.appendChild parent elem) (! elem) #(.remove elem)))))
#_
(e/defn With [elem Body]
  (binding [node (e/input (appending> elem node))]
    node ; P: electric is lazy so if no one uses the node it might not mount.
                                        ; This consumes `node` as per do semantics. G: Is this a hack? I don't
                                        ; like (do) being used to force effects. We will revisit after
                                        ; e/mount-point.
    ($ Body)))


;; * Questions for Leo
;; ** …
