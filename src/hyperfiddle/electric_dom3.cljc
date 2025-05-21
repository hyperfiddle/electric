;; TODO port dom2/Hovered?
;; TODO port dom2/visibility-state
;; TODO polish DOM3 API - esp. events

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
;; - `dom/on!` replaced by `EventListener`

(ns hyperfiddle.electric-dom3
  (:refer-clojure :exclude [comment time])
  (:require
   [clojure.core :as cc]
   [contrib.missionary-contrib :as mx]
   [hyperfiddle.electric3 :as e :refer [$]]
   ;; [hyperfiddle.electric-dom3-events :as events]
   [hyperfiddle.electric-dom3-props :as props]
   [hyperfiddle.electric-dom3-events :as events]
   [hyperfiddle.electric.impl.runtime3 :as r]
   [hyperfiddle.incseq :as i]
   [hyperfiddle.incseq.mount-impl :refer [mount]]
   #?(:cljs [hyperfiddle.kvs :as kvs])
   ;; [hyperfiddle.electric.impl.lang3 :as lang]
   [missionary.core :as m]
   )
  #?(:cljs (:require-macros [hyperfiddle.electric-dom3])))

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

(e/declare node)

#?(:cljs
   (let [key (js/Symbol.for "hyperfiddle.dom3.mount-point")]
     (defn get-mount-point [node] (aget node key))
     (defn set-mount-point [node mp] (aset node key mp))
     (defn remove-mount-point [node] (js-delete node key))))

(defn ->text [elem] (when elem (.-textContent elem)))
(defn texts [coll] (mapv ->text coll))

(def mount-items
  (mount
    (fn [element child] #_(prn 'appendChild (hash child) child) (.appendChild element child))
    (fn [element child previous] #_(prn 'replaceChild (hash child) child) (.replaceChild element child previous))
    (fn [element child sibling] #_(prn 'insertBefore (hash child) child) (.insertBefore element child sibling))
    (fn [element child] #_(prn 'removeChild (hash child) child) (.removeChild element child))
    (fn [element i] (let [child (.item (.-childNodes element) i)] #_(prn 'getChildItem element i (hash child)) child))))

#?(:cljs
   (defn attach! [parent-node tag e]
     (assert (instance? js/Node parent-node) "did you forget to bind `dom/node`?")
     (m/observe (fn [!]
                  (! nil)
                  (if-some [mount-point (get-mount-point parent-node)]
                    (do (kvs/insert! mount-point tag e) #(kvs/remove! mount-point tag))
                    (do (.appendChild parent-node e)    #(.remove e)))))))

;;;;;;;;;;
;; Text ;;
;;;;;;;;;;

;; NOTE L:we could implement variadic Text with a conditional on first rest and self recursion
(e/defn Text
  "Mount a DOM TextNode in current `node`, containing stringified `arg`. Return `arg`."
  [arg] ; ^::lang/print-clj-source
  (e/client
    (e/drain
      (let [e (.createTextNode js/document "")]
        (r/do!
          (e/input (attach! node (e/tag) e))
          (set! (.-textContent e) arg))))))

(defmacro text
  "Mount a DOM TextNode in current `node` for each argument in `args`.
   Each TextNode will contain the stringified argument. Returns nothing."
  ([] `(e/amb))
  ([& args] `(r/do! ~@(for [arg args] `($ Text ~arg)))))

;;;;;;;;;;;;;
;; Comment ;;
;;;;;;;;;;;;;

(e/defn Comment
  "Mount a DOM Comment in current `node`, containing stringified `arg`. Return `arg`."
  [arg] ; ^::lang/print-clj-source
  (e/client
    (e/drain
      (let [e (.createComment js/document "")]
        (r/do!
          (e/input (attach! node (e/tag) e))
          (set! (.-textContent e) arg))))))

(defmacro comment
  "Mount a DOM Comment in current `node` for each argument in `args`.
   Each Comment node will contain the stringified argument. Returns nothing."
  ([] `(e/amb))
  ([& args] `(r/do! ~@(for [arg args] `($ Comment ~arg)))))

;;;;;;;;;;;;;
;; Element ;;
;;;;;;;;;;;;;

(defn perform-additions!
  "Take a DOM `element` and an incseq's diff. Perform addition of new child elements into `element`."
  [element {:keys [degree      ; Max size of collection (after grow, before shrink)
                   grow        ; Number of added items
                   permutation ; Map of indices movements – e.g. "a replaced by b" or "target index -> source index"
                   change      ; Map of index -> value – i.e. child position -> child element object
                   ]}]
  ;; Starts with size before additions and iterates up to degree – scanning rightwards.
  ;; Image:  [a b c d e] ... f g h i]
  ;;                   ^            ^
  ;;        current size            desired size after patch
  ;;        = degree - grow         = degree
  ;;                                must add `f`, `g`, `h`, and `i` from `e` up to `degree`
  (let [move (i/inverse permutation)] ; considered O(1)
    ;; i.e. If current size is 5 and desired size is 10, generate a range (6 7 8 9 10) of new indices
    ;; and perform addition effect for each new index. If there is no size difference, generates an
    ;; empty range, so no effect is performed.
    (reduce (fn [change i] ; reduce over map of diff's changes (a map index -> value), for each new index.
              (let [j (get move i i)] ; resolve index in the `move` map because child element might
                                      ; have been added and moved in a single diff. If it was just
                                      ; added but not moved, keep index as is.
                (.appendChild element (get change j)) ; perform effect
                (dissoc change j))) ; consume this change.
      change
      (range
        (- degree grow)  ; size of the collection before we patch this diff
        degree           ; desired size of the collection after we apply this patch
        ))))

(defn perform-replacements!
  "Take a DOM `element`, an incseq's diff and a map of index -> value representing
  the remaining changes after additions have been performed (return value of
  `perform-additions!`). For each change, will punch new values in, at their
  respective indices, overwriting existing ones. Replacements are not reorders,
  see `perform-reorders!`."
  [element
   {:keys [permutation]}
   remaining-changes ; a map index -> value, return value of `perform-additions!`
   ]
  (let [move (i/inverse permutation)] ; considered O(1)
    (run! (fn [[index new-child-element]] ; for each index -> new child element
            (->> (get move index index)) ; resolve index in the `move` map because child element might
                                         ; have been changed and moved in a single diff. If it was just
                                         ; changed but not moved, keep index as is.
            (.item (.-childNodes element)) ; Get old child element object in current list of element's children.
            (.replaceChild element new-child-element)) ; Replace old child element by new child element. (signature is element.replaceChild(new, old).
      remaining-changes)))

(defn perform-adds-and-replacements!
  "Take a DOM `element` and an incseq's diff. Perform additions of new child elements and replacement of existing ones.
  Replacement is not moving, replacement means replace existing children by new
  children at the same, respective locations."
  ;; NOTE dom/Element's impl mounts nodes in a static order, so no elements get replaced. But one could
  ;; replace elements by rebinding dom/node to an e/watch or conditional value instead.
  [element diff]
  (->> (perform-additions! element diff)    ; consumes diff's changes for additions, return remaining changes.
       (perform-replacements! element diff) ; consumes remaining changes after additions.
       ))

(defn perform-removals!
  "Take a DOM `element` and an incseq's diff. Must be called after `perform-additions!`.
  Will remove all deleted child elements from the `element`'s childlist.
  Return the remaining permutations map to perform (reorders) after removals."
  ;; Starts with size after additions (degree) and iterates down to the final size (after deletions) – scanning leftwards.
  ;; Image:  [a b c d e] f g h i]  ; we delete b d g h.
  ;;            ×   ×  ↑   × ×  ↑
  ;;                   │        │
  ;;             final size     └ size before patch
  ;;          = degree - shrink     = degree
  ;;                                must remove `h`, `g`, `d`, and `b`.
  ;;                                Note removing a DOM Element's child causes a left shift
  ;;                                of all remaining children.
  [element {:keys [permutation
                   degree      ; Max size of collection (after grow, before shrink)
                   shrink      ; Number of removed items
                   ]}]

  (->> (range degree (- degree shrink))
    (reduce (fn [permutation index]
              (let [j (get permutation index index)] ; Resolve index of element to remove. If element
                                                     ; is not in the `permutation` map, then we are
                                                     ; removing the last item of the collection (pop)
                (->> (.item (.-childNodes element) j) ; Get child element object by index
                  (.removeChild element))
                ;; Consume this removal from `permutation` map. This is a Group Theory trick. The
                ;; permutation map forms a Group (think "ring"), as in 0 -> 1 -> 2 -> 0 (notice the
                ;; loop to 0). To remove an item from this ring, we need to shrink the ring by one
                ;; slot, so there's no hole in the ring. By composing a permutation by its
                ;; inverse (e.g. {a b} ∘ {b a}), we form an identity (e.g. {}), and therefore cancels
                ;; it out of the current permutation map.
                (i/compose permutation (i/rotation index j))
                ))
      permutation)))

(defn perform-reorders!
  "Take a DOM `element`, an incseq `diff` and a map of `permutations` describing
  how to reorder child elements of `element`, and perform reordering of those
  child elements. `permutations` must be the return value of `perform-removals!`."
  [element {:keys []} permutations]
  (let [children (.-childNodes element)]
    (loop [permutations permutations] ; loop until
      (when-not (= {} permutations)   ; there are no more permutations to perform
        (let [[i j] (first permutations)     ; for each permutation
              i'     (if (< j i) (inc i) i)] ; if j is left of i, shift i right so to insert to the right of the target position.
          ;; If `i` points to the last element, (inc i) would be OOB. but `.item` would return nil and
          ;; `insertBefore` will interpret nil as insert in last position. NOTE could we use
          ;; replaceChild or replaceWith instead? not clear about pros/cons.
          (.insertBefore element  ; Inserts the item at position j before the item at position i, adjusting if necessary.
            (.item children j)    ; Get child element at position j
            (.item children i'))  ; Get child element at position i (potentially shifted)
          ; Consume this permutation. Detailed explanation in `perform-removals!`'s comments.
          (recur (i/compose permutations (i/rotation i j))))))))

(defn perform-removals-and-reorders!
  "Take a DOM `element` and an incseq's diff. Perform removal of extra (deleted)
  elements, and reordering of remaining elements. Must be called after
  `perform-adds-and-replacements!`."
  [element diff]
  (->> (perform-removals! element diff)
    (perform-reorders! element diff)))


;; Why we can't generalize over any vector-like datatype:
;;      This impl is overspecialized to DOM's NodeList type.
;;      G argued this could be generalized over any vector.
;;      Pro: having a vector impl would allow us to unit test sooner.
;;      an extend protocol would port it to NodeList.
;;  Hypothesis rejected because patching a vector is essentially different than patching a NodeList
;;  Patching a vector is simpler:
;;   - turn the vector into a transient
;;   - grow the vector up to degree by padding it with nils
;;   - perform all permutations, all values to be deleted ends at the end.
;;   - pop! on the vector until it matches the final size
;;   - apply remaining changes (replacements)
;;  See i/patch-vec
;;  This efficient strategy doesn't work for a NodeList, because the DOM is live.
;;  We can't pad a NodeList with nils and we cannot reorder it before we shrink it.
;;  Otherwise there's a risk users might see an inconsistent DOM state.
;;  Also while we patch a NodeList, the manipulation API is on the Node class. So we must pass a Node,
;;  which is not vector-like.
(defn patch-nodelist
  "Take a DOM `element`, an incseq's `diff` and patch the diff over the element's
  children list (a NodeList), applying additions, replacements, removals and reordering of
  children. Must be called exactly once per element and exactly once per diff."
  [element diff]
  ;; Order matters:
  ;; 1. Add new elements,
  ;; 2. Replace existing element,
  ;; 3. Remove retracted elements,
  ;; 4. Reorder remaining elements.
  (perform-adds-and-replacements! element diff)
  (perform-removals-and-reorders! element diff)
  element)

#?(:cljs
   (defn create-element [ns tag mp]
     (doto (if ns
             (.createElementNS js/document ns (name tag))
             (.createElement js/document (name tag)))
       (set-mount-point mp))))

(e/defn With
  "Run `Body` in provided DOM `element`, attaching and managing children inside it.
  One would use `With` instead of `WithElement` to mount an Electric DOM UI inside
  an existing DOM Element, typically libraries integration."
  [element Body] (e/client (binding [node element] ($ Body))))

(e/defn With-element
  "Mount a new DOM Element of type `tag` in the current `node` and run `Body` in
  the context of the new Element."
  ([tag Body] ($ With-element nil tag Body))
  ([ns tag Body]
   (let [mp   (e/client (e/mount-point))
         elem (e/client (create-element ns tag mp))]
     (r/do!
       (e/client
         (r/do!                         ; why e/drain doesn't work here?
           (e/input (attach! node (e/tag) elem))
           (e/input (m/reductions mount-items elem mp))
           nil))
       (binding [node elem] (Body))))))

;; DONE what should `element*` return?
;; - nil :: no because we want UI to produce values
;; - ∅ :: no because we want UI to produce values (see `drain`)
;; - last element - do :: maybe, retrocompatible with dom2
;; - e/amb of all children :: maybe, because why not?

;; As per UI5 TodoMVC use cases, we only want some values to concatenate and bubble up through the
;; return channel (txs only), but not all. We want to avoid having to manually filter out dom/text,
;; dom/comment, and other values returned by cosmetic-only bits of UI. We think we want `do`: `do` will
;; return the last element by default. Userland can call `e/amb` to return more than one value.
;; UI5 TooMVC concat use cases:
;; A. concat two txs (e.g. (concat (Checkbox. ) (TextInput.)) to emit txs about a row.
;; B. apply concat (for [entity query-result]) to concat all text for all rows.

;; A. is (e/amb (do …) (do …))
;; B. is (e/cursor [entity (query-result)] …) because e/cursor returns the concatenation of all body branches.
;;    as if it was `apply e/amb`.
;; Note `(do a b c)` expands to (e/amb (e/drain a) (e/drain b) c), e/drain returns ∅.
(defn element*
  ([tag forms] (element* nil tag forms))
  ([ns tag forms] `($ With-element ~ns ~tag (e/fn [] ~@forms))))

(defmacro element
  "Mount a new DOM Element of type `tag` in the current `node` and run `body` in
  the context of the new Element.
```clj
  (dom/element :div (dom/text \"content\"))
```
"
  [tag & body]
  (element* tag body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Props, Attributes, Styles ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro props
  "
Take a map of HTML attributes to values and reactively sets each of them onto
a given DOM `node`. Default `node` is the one in scope.

Example:
```clj
  (dom/div (dom/props {:id \"my-div\", :class [\"foo\"], :style {:background-color :red}}))
```

- A value of `nil` will remove the attribute.
- Attribute names are case-insensitive, like in HTML.
- Attribute inherits the `node`'s namespace (e.g. SVG vs HTML attributes)
- `:class`, setting the CSS class, can be a string or a collection of strings.
- `:style`, setting inline CSS styles, supports setting CSS variables (e.g. {:--my-color :red})
  - for more complex styles (e.g. pseudo-classes, pseudo-elements, keyframes) use Electric-CSS.

Note `props` will decide if an attribute is set as an HTML attribute or as a DOM
object property. For instance:
- An input's `:value` is set through the `node.value` property.
- `:list` (input's datalist) can only be set by attribute, as the corresponding property is readonly.
- `:class` doesn't set the \"class\" HTML attribute, but efficiently manipulates the node's `.classList` property.
- `:style` doesn't set the \"style\" HTML attribute, but efficiently manipulates the CSSStyleDeclaration object under the `.style` property.
- etc.
  "

  ;; Leo: alternative to class taking a vector: use v3's e/amb or e/diff-by (or any incseq).
  ([attributes] `(props/props ~attributes))
  ([node attributes] `(props/props ~node ~attributes)))

(cc/comment
  ;; Leo: static props
  (props {:class (e/amb "foo" "bar")}) ; instead of "foo bar" or ["foo" "bar"]
  (props {:class (e/diff-by identity ["foo" "bar"])}) ; instead of "foo bar" or ["foo" "bar"]
  ;; Leo: dynamic props

  (Button.
    (if running?
      (props {:class "button-spinning"})
      (props …)))

  (e/defn Button [props]
    (props (cond disabled? (assoc props :disabled true))))

  ;; Leo: what if we used incseq for dynamic props
  (e/amb [:prop1 "value1"], (if <cond> [:prop2 "value2"] (e/amb))) ; pattern: (if <cond> foo (e/amb)) => (e/where <cond> foo)
  ;; or
  (e/diff-by key {:prop1 "value1", :prop2 "value2"})
  ;; Then we can have a function mounting each incseq's instance in the DOM
  ;; e/cursor + m/observe
  ;; NOTE G: we cannot sort an incseq and we need to account for `ordered-props` bug (see props ns)

  )

;; NOTE Leo: beware, watching a dom attribute instead of passing the value
;; around in Electric could introduce glitches as the attribute being set value
;; will trigger a new propagation on the next turn.
(e/defn ^:no-doc Attribute
  "
Watch an `attribute`'s value for a given DOM `node`. Only DOM attributes are
watchable, not object properties. For instance, to watch an input's value, use
`EventListener`. Use `Attributes` to watch multiple attributes at once."
  [node attribute-name]
  ($ props/Attribute node attribute-name))

(e/defn ^:no-doc Attributes
  "
Take a collection of `attribute-names` and watch for attribute changes in
`node`. Return a map of {\"attribute-name\" attribute-value, ...}. Only DOM
attributes are watchable, not object properties. For instance, to watch an
input's value, use `EventListener`."
  [node attribute-names]
  ($ props/Attribute node attribute-names))

;;;;;;;;;;;;
;; Events ;;
;;;;;;;;;;;;

;; Not inlined on purpose. electric core's clock listen to DOM's visibilityState and only ticks when visible.
#?(:cljs (def with-listener events/with-listener))
#?(:cljs (def listen events/listen))
#?(:cljs (def listen-some events/listen-some))

(defn mix-incseq-with-uninitialized-flow ; questionable
  [v events] ; [incseq flow]
  (->> (m/ap
         (let [!x (atom [])] ; store latest value
           (m/amb=
             (do (m/?> (i/latest-product #(reset! !x [%]) v)) (m/amb)) ; extract latest value from v - bypass on empty diff
             (do (reset! !x [(m/?> events)]) (m/amb)) ; run events flow - uninitialized
             (m/?> (m/watch !x))))) ; take latest value
    (i/diff-by {}))) ; return latest of v or events. return empty diff on uninitialized v and event

(e/defn Event-plus-authoritative-value
  [node event-type f v opts]
  (e/client
    (e/join ; incseq -> table
      (mix-incseq-with-uninitialized-flow ; we'd like to avoid this
        (e/pure v)        ; not just init-v, can be controlled v e.g. from db
        (events/listen node event-type ((e/capture-fn) f) opts))))) ; don't rebuild flow when v updates

(e/defn On* ; advanced use case
; This docstring is for the electric team.
"Listen to DOM events of `event-type` on `node`. Default to current dom/node.

Return the return value of `(f event)` for all successive events, starting with `init-v`.
`init-v` is assumed to be constant. If `init-v` changes the underlying event listener will reboot and `On*` will emit
`init-v` again, until the next event.

Use `f` to extract event.target.value, because event.target is often the same object between two events and electric will work-skip.
`f` is a good place to call `event.stopPropagation` or `event.preventDefault`.

`opts` is passed to addEventlistener.
"
  ([event-type f init-v]      (On* node event-type f init-v {}))
  ([event-type f init-v opts] (On* node event-type f init-v opts))
  ([node event-type f init-v opts] ; init-v is supposed to be constant, a variable init-v will reboot the event listener
   (e/client (e/input (m/reductions {} init-v (events/listen node event-type f opts))))))

(defmacro On
  ([event-type f v]      `(On node ~event-type ~f ~v {}))
  ([event-type f v opts] `(On node ~event-type ~f ~v ~opts))
  ([node event-type f v opts]
   `(let [v# ~v, opts# ~opts, e# ~event-type
          f# (e/client ~f)] ; auto-site f, a common gotcha when calling dom/On from site-neutral code
      ; f is unserializable, and called on client, so this is safe unless expr f is complex,
      ; e.g. a server sited factory building a client sited callback
      (e/client ; remove call latency when server sited
        (Event-plus-authoritative-value ~node e# f# v# opts#)))))

(defn fork
  ([flow] (fork ##Inf flow))
  ([n flow]
   (m/ap
     (let [S (i/spine) !running (atom (sorted-set))] ; both refs are ordered, entires must be comparable
       (m/amb S
         (let [[i v] (m/?> (m/eduction (map-indexed vector) flow)) ; spine is ordered by event sequence
               t (fn token
                   ([] (swap! !running disj i) (S i {} nil))
                   ([err] '...))
               running (swap! !running conj i)]
           (S i {} [t v])
           ; enforce concurrency limit
           (run! #(S % {} nil) (take (- (count running) n) ; NOTE Leo: always return 0 or 1 because we add one event at a time
                                 running))
           (m/amb)))))))

; FAQ: can fork's token support the error arity? NO, the lifetime of the error and
; the token are different! The token must be consumed on error so that the user
; can interact again, but the error must be retained for display. How can we model this?

(e/defn On-all
  ([event-type]                           ($ On-all      event-type identity))
  ([event-type f]                         ($ On-all      event-type f        {}))
  ([event-type f opts]                    ($ On-all      event-type f        opts ##Inf))
  ([event-type f opts concurrency-factor] ($ On-all node event-type f        opts concurrency-factor))
  ([node event-type f opts concurrency-factor]
   (e/client (e/join (e/input (fork concurrency-factor (events/listen-some node event-type ((e/capture-fn) f) opts)))))))

;;;;;;;;;;;;
;; Extras ;;
;;;;;;;;;;;;

#?(:cljs (defn squarewave [node enter-event leave-event init f]
           (->> (mx/mix
                  (m/observe (fn [!] (events/with-listener node enter-event (fn [e] (some-> (f enter-event e) !)))))
                  (m/observe (fn [!] (events/with-listener node leave-event (fn [e] (some-> (f leave-event e) !))))))
             (m/reductions {} init) (m/relieve {}))))

(e/defn Mouse-over?
  ([] (Mouse-over? node))
  ([node] (e/client (e/input (squarewave node "mouseenter" "mouseleave" false {"mouseenter" true, "mouseleave" false})))))

(e/defn Focused?
  ([] (Focused? node))
  ([node] (e/client (e/input (squarewave node "focus" "blur" (= node (.-activeElement js/document)) {"focus" true, "blur" false})))))

(e/defn Focused-in?
  ([] (Focused-in? node))
  ([node] (Focused-in? node (fn [event-type ^js event]
                              ;; focus from outside, blur to outside of container – ignore internal focus/blur between children.
                              (when (not (.contains node (.-relatedTarget event)))
                                ({"focusin" true, "focusout" false} event-type)))))
  ([node f]
   (e/client (e/input (squarewave node "focusin" "focusout" (= node (.-activeElement js/document)) f)))))

(e/defn Mouse-down?
  ([] (Mouse-down? node))
  ([node] (e/client (e/input (squarewave node "mousedown" "mouseup" false {"mousedown" true, "mouseup" false})))))

;;;;;;;;;;;
;; Sugar ;;
;;;;;;;;;;;

(defmacro a [& body] (element* :a body))
(defmacro abbr [& body] (element* :abbr body))
(defmacro address [& body] (element* :address body))
(defmacro area [& body] (element* :area body))
(defmacro article [& body] (element* :article body))
(defmacro aside [& body] (element* :aside body))
(defmacro audio [& body] (element* :audio body))
(defmacro b [& body] (element* :b body))
(defmacro bdi [& body] (element* :bdi body))
(defmacro bdo [& body] (element* :bdo body))
(defmacro blockquote [& body] (element* :blockquote body))
(defmacro br [& body] (element* :br body))
(defmacro button [& body] (element* :button body))
(defmacro canvas [& body] (element* :canvas body))
(defmacro cite [& body] (element* :cite body))
(defmacro code [& body] (element* :code body))
(defmacro col [& body] (element* :col body))
(defmacro colgroup [& body] (element* :colgroup body))
(defmacro data [& body] (element* :data body))
(defmacro datalist [& body] (element* :datalist body))
(defmacro dd "The <dd> HTML element provides the description, definition, or value for the preceding term (<dt>) in a description list (<dl>)." [& body] (element* :dd body))
(defmacro del [& body] (element* :del body))
(defmacro details [& body] (element* :details body))
(defmacro dfn [& body] (element* :dfn body))
(defmacro dialog [& body] (element* :dialog body))
(defmacro div [& body] (element* :div body))
(defmacro dl "The <dl> HTML element represents a description list. The element encloses a list of groups of terms (specified using the <dt> element) and descriptions (provided by <dd> elements). Common uses for this element are to implement a glossary or to display metadata (a list of key-value pairs)." [& body] (element* :dl body))
(defmacro dt "The <dt> HTML element specifies a term in a description or definition list, and as such must be used inside a <dl> element. It is usually followed by a <dd> element; however, multiple <dt> elements in a row indicate several terms that are all defined by the immediate next <dd> element." [& body] (element* :dt body))
(defmacro em [& body] (element* :em body))
(defmacro embed [& body] (element* :embed body))
(defmacro fieldset [& body] (element* :fieldset body))
(defmacro figure [& body] (element* :figure body))
(defmacro footer [& body] (element* :footer body))
(defmacro form [& body] (element* :form body))
(defmacro h1 [& body] (element* :h1 body))
(defmacro h2 [& body] (element* :h2 body))
(defmacro h3 [& body] (element* :h3 body))
(defmacro h4 [& body] (element* :h4 body))
(defmacro h5 [& body] (element* :h5 body))
(defmacro h6 [& body] (element* :h6 body))
(defmacro header [& body] (element* :header body))
(defmacro hgroup [& body] (element* :hgroup body))
(defmacro hr [& body] (element* :hr body))
(defmacro i [& body] (element* :i body))
(defmacro iframe [& body] (element* :iframe body))
(defmacro img [& body] (element* :img body))
(defmacro input [& body] (element* :input body))
(defmacro ins [& body] (element* :ins body))
(defmacro itemprop [& body] (element* :itemprop body))
(defmacro kbd [& body] (element* :kbd body))
(defmacro label [& body] (element* :label body))
(defmacro legend [& body] (element* :legend body))
(defmacro li [& body] (element* :li body))
(defmacro link [& body] (element* :link body))
(defmacro main [& body] (element* :main body))
#_ (defmacro map [& body] (element* :map body))
(defmacro mark [& body] (element* :mark body))
(defmacro math [& body] (element* :math body))
(defmacro menu [& body] (element* :menu body))
(defmacro meter [& body] (element* :meter body))
(defmacro nav [& body] (element* :nav body))
(defmacro noscript [& body] (element* :noscript body))
(defmacro object [& body] (element* :object body))
(defmacro ol [& body] (element* :ol body))
(defmacro optgroup [& body] (element* :optgroup body))
(defmacro option [& body] (element* :option body))
(defmacro output [& body] (element* :output body))
(defmacro p [& body] (element* :p body))
(defmacro picture [& body] (element* :picture body))
(defmacro pre [& body] (element* :pre body))
(defmacro progress [& body] (element* :progress body))
(defmacro q [& body] (element* :q body))
(defmacro ruby [& body] (element* :ruby body))
(defmacro s [& body] (element* :s body))
(defmacro samp [& body] (element* :samp body))
(defmacro script [& body] (element* :script body))
(defmacro section [& body] (element* :section body))
(defmacro select [& body] (element* :select body))
(defmacro slot [& body] (element* :slot body))
(defmacro small [& body] (element* :small body))
(defmacro span [& body] (element* :span body))
(defmacro strong [& body] (element* :strong body))
(defmacro style [& body] (element* :style body))
(defmacro sub [& body] (element* :sub body))
(defmacro summary [& body] (element* :summary body))
(defmacro sup [& body] (element* :sup body))
(defmacro table [& body] (element* :table body))
(defmacro tbody [& body] (element* :tbody body))
(defmacro td [& body] (element* :td body))
(defmacro template [& body] (element* :template body))
(defmacro textarea [& body] (element* :textarea body))
(defmacro th [& body] (element* :th body))
(defmacro thead [& body] (element* :thead body))
(defmacro time [& body] (element* :time body))
(defmacro tr [& body] (element* :tr body))
(defmacro u [& body] (element* :u body))
(defmacro ul [& body] (element* :ul body))
(defmacro var [& body] (element* :var body))
(defmacro video [& body] (element* :video body))
(defmacro wbr [& body] (element* :wbr body))

#?(:cljs (defn await-element-first-match-only [node selector]
           (fn [s f]
             (let [o (js/MutationObserver.
                       (fn [ms o]
                         (try
                           (when-let [x (.querySelector node selector)]
                             #(.disconnect o) ; stop listening after first detection
                             (s x))
                           (catch :default e (f e)))))]
               (try (.observe o node #js{:subtree true :childList true})
                 (catch :default e (f e)))
               #(.disconnect o)))))

#?(:cljs (defn await-element "
return flow of elements matching selector over time, i.e. as the target element comes and goes
we need to keep listening for when it comes back"
           [node selector]
           (m/relieve
             (m/observe
               (fn [!]
                 (! (.querySelector node selector))
                 (let [o (js/MutationObserver.
                           (fn [ms o]
                             (! (.querySelector node selector))
                             ; todo use querySelectorAll and return simultaneous matches as table
                             #_(doseq [x (array-seq (.querySelectorAll node selector))] (! x))))]
                   (try (.observe o node #js{:subtree true :childList true})
                        (catch :default e (js/error "MutationObserver failed, invalid call? e: " e)))
                   #(.disconnect o)))))))

(e/defn Await-element [node selector] ; reactive to elements coming and going
  ; this is expensive, so don't provide the js/document.body default node
  (e/client (e/input (await-element node selector))))
