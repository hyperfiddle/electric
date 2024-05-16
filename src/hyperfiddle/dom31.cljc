;; * DONE Replace dom3 by dom3_efn
;;   G: diffed both files, LGTM
;; * DONE move event handling to separate ns
;;   So we can think clearly
;;   We can always merge back later
;; * DONE Implement dom/text
;; * DONE Implement dom/comment
;; * DONE Implement dom/div
;; * DONE Implement dom/div nesting
;; * TODO Implement setting attributes
;; * TODO Implement setting class
;; * TODO Implement setting inline style
;; * TODO Implement event handling

(ns hyperfiddle.dom31
  (:refer-clojure :exclude [comment])
  (:require
   [hyperfiddle.electric-de :as e :refer [$]]
   ;; [contrib.assert :as ca]
   [hyperfiddle.rcf :as rcf :refer [tests]]
   [missionary.core :as m]
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

;; TODO Understand this with 100% precision.
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

;;;;;;;;;;;
;; Sugar ;;
;;;;;;;;;;;

(defmacro div [& body] (element* "div" body))

;;;;;;;;;;;;;
;; CLASSES ;;
;;;;;;;;;;;;;

(defn parse-class [xs]
  (cond (or (string? xs) (keyword? xs) (symbol? xs)) (re-seq #"[^\s]+" (name xs))
        (or (vector? xs) (seq? xs) (list? xs) (set? xs)) (into [] (comp (mapcat parse-class) (distinct)) xs)
        (nil? xs) nil
        :else (throw (ex-info "don't know how to parse into a classlist" {:data xs}))))

(tests
  (parse-class "a") := ["a"]
  (parse-class :a) := ["a"]
  (parse-class 'a/b) := ["b"]
  (parse-class "a b") := ["a" "b"]
  (parse-class ["a"]) := ["a"]
  (parse-class ["a" "b" "a"]) := ["a" "b"]
  (parse-class ["a" "b"]) := ["a" "b"]
  (parse-class ["a b" "c"]) := ["a" "b" "c"]
  (parse-class [["a b"] '("c d") #{#{"e"} "f"}]) := ["a" "b" "c" "d" "e" "f"]
  (parse-class nil) := nil
  (parse-class "") := nil
  (parse-class " a") := ["a"]
  (try (parse-class 42) (throw (ex-info "" {}))
       (catch ExceptionInfo ex (ex-data ex) := {:data 42})))

#?(:cljs
   (defn build-class-signal [node clazz]
     (m/signal (m/observe (fn [!]
                            (! nil)
                            (.add (.-classList node) clazz)
                            #(.remove (.-classList node) clazz))))))
#?(:cljs
   (defn get-class-signal [node clazz]
     (let [k (js/Symbol.for (str "hyperfiddle.dom3.class-signal-" clazz))]
       (or (aget node k) (aset node k (build-class-signal node clazz))))))

(e/defn Class [node clazz] (e/client (e/input (get-class-signal node clazz))))

;; how to run an e/fn over a clojure sequence
(e/defn MapCSeq [Fn cseq] (e/cursor [[_ v] (e/diff-by first (map-indexed vector cseq))] ($ Fn v)))
(defmacro for-cseq [[b cseq] & body] `(e/cursor [[i# ~b] (e/diff-by first (map-indexed vector ~cseq))] ~@body))

(e/defn ClassList [node classes]
  (e/client
    ($ MapCSeq (e/fn [clazz] ($ Class node clazz)) (parse-class classes))
    #_(for-cseq [clazz (parse-class classes)] ($ Class node clazz))
    ))



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
