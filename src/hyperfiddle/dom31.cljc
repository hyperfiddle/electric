;; * DONE Replace dom3 by dom3_efn
;;   G: diffed both files, LGTM
;; * DONE move event handling to separate ns
;;   So we can think clearly
;;   We can always merge back later
;; * DONE Implement dom/text
;; * DONE Implement dom/comment
;; * DONE Implement dom/div
;; * DONE Implement dom/div nesting
;; * DONE Implement setting attributes
;; * DONE Implement setting class
;; * DONE Implement setting inline style
;; * TODO Implement event handling

;;; Breaking changes:
;; - `dom/style` now creates a <style> element. It used to set inline CSS style onto a dom node.
;;   Users had to resort to (dom/element "style" …) to create a <style> node.
;;   How to set styles:
;;     - Set inline styles with `(dom/props {:style {:color :red}})`, see `dom/props`
;;     - Add an external stylesheet with: `<link rel="stylesheet" href="…"/>`
;;     - If you need to manipulate complex CSS rules, don't concatenate strings in a <style> node.
;;       Instead use `electric-css`:
;;       e.g. (css/scoped-style (css/rule ".my-class::before:hover" {:--my-css-color-var :red}))
;; - `dom/comment_` has been renamed to `dom/comment` (reactive HTML comment)

(ns hyperfiddle.dom31
  (:refer-clojure :exclude [comment])
  (:require
   [hyperfiddle.electric-de :as e :refer [$]]
   ;; [contrib.assert :as ca]
   [hyperfiddle.rcf :as rcf :refer [tests]]
   [missionary.core :as m]
   [hyperfiddle.dom31-attributes :as attrs]
   ;; [hyperfiddle.electric.impl.lang-de2 :as lang]
   ;; #?(:cljs [goog.dom])
   )
  #?(:clj (:import [clojure.lang ExceptionInfo])))

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
   (let [key (js/Symbol.for "hyperfiddle.dom3.mount-point")]
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

;; NOTE L:we could implement variadic Text with a conditional on first rest and self recursion
(e/defn Text [str] ; ^::lang/print-clj-source
  (e/client
    (let [e (.createTextNode js/document "")]
      (e/input (attach! node (e/tag) e))
      (set! (.-textContent e) str)
      ;; TODO return string? or e? or nil?
      )))

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

;; DONE Understand this with 100% precision.
;; TODO split into simpler components
;; G: here is my understanding of it (as comments), to be verified.
;; Leo says its done
(defn mount-items ;; must not be called on the same element more than once.
  [element ; A dom element to mount children in

                       ; An incseq's diff
   {:keys [grow        ; Number of added items
           shrink      ; Number of removed items
           degree      ; Max size of collection (after grow, before shrink)
           permutation ; Map of indexes movements e.g. "a replaced by b" "target -> source"
           change      ; Map of index -> value (dom elements in this case)
           ]}]
  (let [children    (.-childNodes element)  ; Current children of the element to mount in
        move        (i/inverse permutation) ; map of source -> target aka "b replaces a"
        size-before (- degree grow)         ; Current expected child list size. Should be exactly (alength children)
        size-after  (- degree shrink)       ; Expected child list size after patch
        ]
    ;; Step 1 - Additions and replacements by new elements.

    ;; NOTE that Element's impl mounts nodes in a static order, so no elements
    ;; get replaced. But one could replace elements by rebinding dom/node to an
    ;; e/watch or conditional value instead.

    ;; Starts with the size before additions and iterates up to degree, applying
    ;; changes (additions or replacements).
    (loop [i      size-before
           change change]
      (if (not (== i degree)) ; Checks if there are items to add or replace.
        ;; Addition
        (let [j (get move i i)]                 ; Index of new child. If the added element has not moved, just use the current index (add it to current slot)
          (.appendChild element (get change j)) ; Appends the new child to the element.
          (recur (inc i) (dissoc change j)))    ; Continues with the next item, removing the processed item from the change map.
        ;; If there are changes but no items to add, then it's just in-place changes.
        (run! (fn [[i new-element]] ; Iterates over the remaining changes and applies replacements.
                (.replaceChild element new-element   ; Replaces the old child at the specified index with the new child.
                  (.item children   ; Get old child object
                    (get move i i)) ; Get old child index
                  ))
          change)))
    ;; Step 2 - Removals and reorders of existing elements
    (loop [permutation permutation
           i           degree]
      (if (not (== i size-after)) ; If there are extra items
        ;; Removals
        (let [i (dec i) ; Move to the penultimate item index - next last index after removal
              j (get permutation i i)] ; Determines the index of the item to be removed.
          (.removeChild element (.item children j)) ; Remove child - causing a left shift
          (recur (i/compose permutation (i/rotation i j)) i)) ; Continues with the next item, updating the permutation to reflect the left shift.
        ;; Reorders
        (loop [permutation permutation]
          (when-not (= permutation {}) ; Checks if there are swaps to apply.
            (let [[i j] (first permutation)] ; Applies the first swap in the permutation.
              ;; If `i` points to the last element, (inc i) would be OOB. but .item would return nil, so insertBefore will interpret nil as insert in last position.
              ;; NOTE could we use replaceChild or replaceWith instead? not clear what are pros/cons.
              (.insertBefore element (.item children j)  ; Inserts the item at position j before the item at position i, adjusting if necessary.
                (.item children (if (< j i) (inc i) i))) ; Due to insertBefore, if j is left of i, insert to the right of the target position.
              ;; Remove applied permutation from permutation map and continue.
              ;; Image: a shrinking yarn loop
              (recur (i/compose permutation (i/rotation i j))))))))
    element))

(e/defn Element [tag Body]
  (e/client
    (let [e   (.createElement js/document (name tag))
          mp  (e/mount-point)
          tag (e/tag)]
      (e/input (attach! node tag e)) ; mount and unmount element in parent
      (e/input (m/reductions mount-items e mp))    ; interprets diffs to mount and maintain children in correct order
      (mount-point e mp)             ; expose mount point to children
      (binding [node e]              ; run continuation, in context of current node.
        ($ Body)))))

(defn element* [tag forms] `($ Element ~tag (e/fn [] (e/amb ~@forms) ; TODO should we use e/amb or do? return all children, concatenated or just the last as in v2?
                                              )))
(defmacro element [tag & body] (element* tag body))

(clojure.core/comment
  (element :div a b)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Props, Attributes, Styles ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro props
  "Take a map of HTML attributes or properties to values and reactively set each of them onto a DOM node.
  Default DOM node is the one in scope.

   e.g. (dom/div (dom/props {:id \"my-div\", :class [\"foo\"], :style {:background-color :red}}))

  - attributes and properties names are case-sensitive.
  - :class can be a string or collection of strings.
  - :style supports setting CSS variables e.g. {:--my-color :red}
    - for more complex styles (e.g. pseudo-classes, pseudo-elements, keyframes) use `electric-css`

  ;; TODO explain that props doesn't support :on-click and link where to look at
   "
  ([m] `(attrs/props ~m))
  ([node m] `(attrs/props ~node ~m)))

(e/defn Attribute
  "Watch an `attribute`'s value for a given DOM `node`. Only DOM attributes are watchable, not object properties.
  Use `Attributes` to watch multiple attributes at once."
  [node attribute-name]
  ($ attrs/Attribute node attribute-name))

(e/defn Attributes
  "Take a collection of `attribute-names` and watch for attribute changes in
  `node`. Return a map of attribute-name (a string) to latest corresponding
  values. Only DOM attributes are watchable, not object properties."
  [node attribute-names]
  ($ attrs/Attribute node attribute-names))

;;;;;;;;;;;
;; Sugar ;;
;;;;;;;;;;;

(defmacro div [& body] (element* "div" body))



(clojure.core/comment
  "comment var is already taken"


  (defn seq->incseq [xs] (apply i/fixed (eduction (map r/invariant) xs)))

  )

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
