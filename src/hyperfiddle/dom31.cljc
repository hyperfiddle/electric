;; * DONE Replace dom3 by dom3_efn
;;   G: diffed both files, LGTM
;; * DONE move event handling to separate ns
;;   So we can think clearly
;;   We can always merge back later
;; * DONE Implement dom/text
;; * TODO Implement dom/comment
;; * TODO Implement dom/div
;; * TODO Implement dom/div nesting
;; * TODO Implement setting attributes
;; * TODO Implement setting class
;; * TODO Implement setting inline style
;; * TODO Implement event handling

(ns hyperfiddle.dom31
  (:require
   [hyperfiddle.electric-de :as e :refer [$]]
   ;; [contrib.assert :as ca]
   [missionary.core :as m]
   ;; [hyperfiddle.electric.impl.lang-de2 :as lang]
   #?(:cljs [goog.dom])))

;; e/tag                                 ;; electric clojure only. resolves to an invariant+singleton unique identifier.
;; (e/mount-point)                       ;; clojure function returning a fresh container associating tags to stateful items which can be observed as an incseq with e/join. The ordering of items reflects the ordering of tags in the program order.

;; (e/insert! mount-point tag init)      ;; add a new item associated with `tag` in `mount-point`, with initial state `init`.
;; (e/update! mount-point tag f & args)  ;; change state of item associated with `tag` in `mount-point` by applying function `f` to current state, with optional following arguments `args`.
;; (e/remove! mount-point tag)           ;; remove item associated with `tag` in `mount-point`.

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

(e/defn Text [str] ; ^::lang/print-clj-source
  (e/client
    (let [e (goog.dom/createTextNode "")]
      (e/input (attach! node (e/tag) e))
      (goog.dom/setTextContent e str))))

(defmacro text [& strs] `(do ~@(for [s strs] `($ Text ~s))))

(comment
  (div
    (text "test"
          (text (str "hello" "world"))))
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
