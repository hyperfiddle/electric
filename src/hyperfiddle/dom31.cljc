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

(defn get-mount-point [node] (aget node "mount-point"))

#?(:cljs
   (defn attach! [parent-node tag e]
     (assert (instance? js/Node parent-node))
     (m/observe (fn [!]
                  (! nil)
                  (let [mount-point (get-mount-point node)] ; TODO could this be inlined?
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

;; TODO this is a sketch, finish impl
(defn mount-items [element {:keys [grow shrink degree permutation change]}]
  (let [children    (.-childNodes element)
        move        (i/inverse permutation)
        size-before (- degree grow)
        size-after  (- degree shrink)]
    (loop [i size-before
           c change]
      (if (== i degree)
        (reduce-kv
          (fn [_ i e]
            (.replaceChild element e
              (.item children (move i i))))
          nil c)
        (let [j (move i i)]
          (.appendChild element (c j))
          (recur (inc i) (dissoc c j)))))
    (loop [p permutation
           i degree]
      (if (== i size-after)
        (loop [p p]
          (when-not (= p {})
            (let [[i j] (first p)]
              (.insertBefore element (.item children j)
                (.item children (if (< j i) (inc i) i)))
              (recur (i/compose p (i/rotation i j))))))
        (let [i (dec i)
              j (p i i)]
          (.removeChild element (.item children j))
          (recur (i/compose p (i/rotation i j)) i))))
    element))

(e/defn Element [tag Body]
  (let [e   (.createElement js/document (name tag))
        mp  (e/mount-point)
        tag (e/tag)]
    (e/input (attach! node tag e)) ; mount and unmount element in parent
    (e/join (e/mount-items e mp))  ; mount children in this node via mount point
    (aset e "mount-point" mp)      ; expose mount point to children ; TODO namespace and hide prop using js/Symbol
    (binding [node e]              ; run continuation
      (Body.))))

(defmacro element [tag & body]
  `($ Element tag (e/fn [] (e/amb ~@body))))

;; (defmacro element [tag & body]
;;   `(let [e# (.createElement js/document ~(name tag))
;;          mp# (e/mount-point)
;;          tag# (e/tag)
;;          parent# (aget node "mount-point")]
;;      (e/insert! parent# tag# e#)                          ;; mount element in parent
;;      (e/on-unmount #(e/remove! parent# tag# e#))          ;; unmount element from parent
;;      (e/join (mount-items node mp#))                      ;; mount children via mount point
;;      (aset e# "mount-point" mp#)                          ;; expose mount point to children
;;      (binding [node e#]                                   ;; run continuation
;;        (e/amb ~@body))))

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
