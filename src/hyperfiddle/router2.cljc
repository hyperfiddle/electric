(ns hyperfiddle.router2
  (:require [hyperfiddle.rcf :as rcf :refer [tests % tap with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom])
  #?(:clj (:import [clojure.lang IRef IAtom]))
  #?(:cljs (:require-macros hyperfiddle.router2)))

;; TODO move bindings to hyperfiddle.api

;; 1: ~have an atom cursor data type~ (see comment below)
;; 1. have a continuous time read-only route value (single m/watch)
;; 2. have a write only api
;; 3. have an history protocol for atom (clojure) and html5 history (cljs)
;; 4. have a Link component calling the private push-state! api.
;; 5. hf/router nil is the top route
;; 6. ~hf/!route is an atom~ !history is atom-like
;; 7. ~nested hf/router calls rebinds hf/!route to a cursor~ (see comment below)
;; 7. nested hf/router calls rebinds hf/route and hf/swap-route!
;; 8. ~a change to the top-level hf/!route binding triggers a replace-state! to the history~
;;    no !history is the single source of truth
;; 9. a push-state! resets the current hf/!history binding.


;; Based on a reflexion of the use of cursors in Photon, I’m challengineg goal 1. because:
;; - A cursor is a read-write interface supporting m/watch.
;; - m/watch is eager, so a change to the watched cursor immedately triggers a reactor propagation turn
;; - a cursor always write to the original root atom
;; - the root atom notifies its cursors recursively and eagerly
;; - therefore all m/watch on the cursor tree will schedule a reactor propagation turn eagerly on every root atom change.
;; - an optimization would be for the cursor watch mechanisme to dedupe by `=` before notifying the m/watch, reimplementing work-skipping in an eager way
;; - glitches would happen at scale, just like in reagent
;; - a loop can happen if a watched cursor triggers a write to another cursor in the same tree
;; - the reactor would run until a fixed point is reached
;; - while the same problem exists with m/watch on atoms, a tree of cursors makes these loops non trivial an hard to debug (is this a strong argument?)

;; By separating the read from the write:
;; we would have a single m/watch on the root atom, ensuring a change triggers one propagation turn.
;; deduping would be lazy (handled by photon)
;; loops would still be possible but would be easier to track (is this a strong argument?)

(comment
  (rcf/enable! true))

;;; History

(defprotocol IHistory
  (navigate! [this route])
  (back! [this])
  (forward! [this])
  (replace-state! [this new-state]))

(declare notify-watches)

(defn updatef [[history idx] f] [(update history idx f) idx])

#?(:clj
   (defrecord AtomHistory [state watches max-size]
     IAtom
     (swap [this f]           (notify-watches this (swap-vals! state updatef f)))
     (swap [this f arg]       (.swap this #(f % arg)))
     (swap [this f arg1 arg2] (.swap this #(f % arg1 arg2)))
     (swap [this f x y args]  (.swap this #(apply f % x y args)))
     (reset [this newval]     (.swap this (constantly newval)))
     (compareAndSet [this oldv newv]
       (loop []
         (let [refv (deref this)]
           (if (not= oldv refv)
             false
             (or (compare-and-set! state refv (updatef refv (constantly newv)))
               (recur))))))

     IRef
     (setValidator [_ _] (throw (UnsupportedOperationException. "History does not support validators")))
     (getValidator [_] (throw (UnsupportedOperationException. "History does not support validators")))
     (getWatches [_] (deref watches))
     (addWatch [this key callback]
       (swap! watches assoc key callback)
       this)
     (removeWatch [_ key] (swap! watches dissoc key))
     (deref [_] (let [[history idx] @state]
                  (get history idx)))
     ))

(defn notify-watches [this [oldstate newstate]]
  (let [oldval (apply get oldstate)
        newval (apply get newstate)]
    (doseq [[key callback] @(:watches this)]
      (callback key this oldval newval))))

;; (add-watch (atom 0) ::key (constantly true))

#?(:clj (defmethod print-method AtomHistory [x w] (print-dup x w)))

#?(:cljs
   (defrecord AtomHistory [state watches max-size]
     IAtom
     ISwap
     (-swap! [this f]           (notify-watches this (swap-vals! state updatef f)))
     (-swap! [this f arg]       (-swap! this #(f % arg)))
     (-swap! [this f arg1 arg2] (-swap! this #(f % arg1 arg2)))
     (-swap! [this f x y args]  (-swap! this #(apply f % x y args)))

     IReset
     (-reset! [this newval]     (-swap! this (constantly newval)))

     IWatchable
     (-add-watch [this key callback]
       (swap! watches assoc key callback)
       this)
     (-remove-watch [_ key] (swap! watches dissoc key))

     IDeref
     (-deref [_] (let [[history idx] @state]
                  (get history idx)))
     ))

(extend-type AtomHistory
  IHistory
  (navigate! [this route]
    (notify-watches this
      (swap-vals! (.-state this)
        (fn [[history idx]]
          (if (= (count history) (.-max-size this)) ; TODO use a ring data structure for O(1) ops
            [(conj (subvec history 1)           route)      idx]
            [(conj (subvec history 0 (inc idx)) route) (inc idx)])))))
  (back! [this]
    (notify-watches this
      (swap-vals! (.-state this) (fn [[history idx]] [history (max (dec idx) 0)]))))
  (forward! [this] (notify-watches this
                     (swap-vals! (.-state this) (fn [[history idx]] [history (min (inc idx) (dec (count history)))]))))
  (replace-state! [this new-state] (.reset this new-state)))


(defn atom-history
  "Return a new IHistory instance backed by an atom.
  Initial history state can be provided with `initial-state`.
  Default history size is unbounded and can be constrained to `max-size` elements in a FIFO way.
  A negative value or 0 has no effect."
  ([] (atom-history nil 0))
  ([initial-state] (atom-history initial-state 0))
  ([initial-state max-size] (->AtomHistory (atom [[initial-state] 0]) (atom {}) max-size)))

(tests
  "navigate"
  (let [h (atom-history)]
    @h := nil
    (navigate! h :a)
    @h := :a))

(tests
  "back and forth"
  (let [h (atom-history)]
    @h := nil
    (navigate! h :a)
    (navigate! h :b)
    @h := :b
    (back! h)
    @h := :a
    (back! h)
    @h := nil
    (forward! h)
    @h := :a
    (navigate! h :c)
    @h := :c
    (forward! h)
    @h := :c))

(tests
  "replace-state"
  (let [h (atom-history)]
    @h := nil
    (replace-state! h :a)
    @h := :a
    (navigate! h :b)
    @h := :b
    (replace-state! h :a)))

(tests
  "max-size and initial value"
  (let [h (atom-history :init 1)]
    @h := :init
    (navigate! h :a)
    @h := :a
    (navigate! h :b)
    @h := :b
    (back! h)
    @h := :b
    ))

(defn history? [h]
  (and (satisfies? IHistory h)
    (instance? IAtom h)))

(tests
  (history? (atom-history)) := true)

;;; 1. and 2.

(p/def !history nil)                    ; History instance mutable ref
(p/def history ::unset)
(p/def route ::unset)
(p/def path [])
(p/def swap-route! nil)

(defn update-in* [m path f & args]
  (if (empty? path)
    (apply f m args)
    (apply update-in m path f args)))

(defn check-route! [route]
  (assert (or (nil? route) (associative? route)) (str "A route should be an associative data structure. Given " route)))

(defn default-platform-history []
  #?(:clj (atom-history)
     :cljs (atom-history)))

(defmacro router
  "
  Evaluates `body` in a routing context, in which:
  - `route` is bound to the current history state,
  - `swap-route!` will set the current route (does not trigger a navigation),
  - `link` will render a dom anchor triggering a soft navigation in the current history.

  `router` expressions can nest, in which case the current route is scoped.
  Page components can therfore safely route locally without conflicting while
  still supporting global back and forward navigation.

  Takes as first argument, either:
  - an `IHistory` instance - typically at the root of your program.
  - an ident (keyword or symbol) to focus on a child route scope.
  - `nil` will pick the default `IHistory` instance for the current
    platform (Browser history or in-memory atom)

  Usage:
  ```
  (router nil                         ; pick default router
     (link {::page ::b} (dom/text \"Go to Page B\"))
     (case (::page route)
       ::b (dom/text \"you are on page b\")
       ::c (dom/text \"you are on page c\"))
     (router ::subpage
       (swap-route! assoc :key :value) ; typically a consequence of a user action
       (link {::page ::c} (dom/text \"Go to Page C\"))))
  ```

  Advanced use case: a nested `router` call can redefine the history in which
  case navigation will occur locally. Note the browser history is a singleton,
  so navigation always occur at the page level. Binding the same history
  instance twice is a noop."

  [history-or-ident & body]
  `(let [history-or-ident# ~history-or-ident
         ident#            (if (ident? history-or-ident#) history-or-ident# nil)
         path#             (if (nil? ident#) [] [ident#])
         [history# path#]  (if (ident? history-or-ident#) ; if we focus on a sub route
                             [!history (into path path#)] ; same history, different path
                             (let [h# (or history-or-ident# (default-platform-history))]
                               (if (= h# !history) ; if we rebind to the same history
                                 [h# path#]        ; noop
                                 [h# []])          ; new history, root path
                               ))]
     (binding [!history history#
               path     path#]
       (binding [history (if (= ::unset history) (p/watch !history) history)]
         (binding [route (let [route (if (= ::unset route) history route)]
                           (check-route! route)
                           (if (some? ident#)
                             (get route ident#)
                             route))]
           (binding [swap-route! (partial (fn [!history# path# & args#]
                                            (swap! !history# (fn [r#] (apply update-in* r# path# args#))))
                                   !history path)]
             ~@body))))))

(tests
  (with (p/run (try (router nil (tap route))
                    (catch Throwable t
                      (prn t))))
    % := nil))

(tests
  (with (p/run (router (atom-history {:a "hello"})
                 (tap route)
                 (router :a
                   (tap route))))
    % := {:a "hello"}
    % := "hello"))

(tests
  (with (p/run (router nil
                 (tap route)
                 (swap-route! assoc :key "top")
                 (router :child
                   (swap-route! assoc :key "nested")))
          )
    % := nil
    % := {:key "top"
          :child {:key "nested"}}
    ))


(tests
  "History + route"
  (let [!command (atom nil)]
    (with (p/run (binding [!history (atom-history)]
                   (router :page
                     (tap route)
                     (let [[command value] (p/watch !command)]
                       (case command
                         ::navigate!      (navigate! !history value)
                         ::back!          (back! !history)
                         ::forward!       (forward! !history)
                         ::replace-state! (replace-state! !history value)
                         nil)))))
      % := nil
      (reset! !command [::navigate! {:page :a}])
      % := :a
      (reset! !command [::navigate! {:page :b}])
      % := :b
      (reset! !command [::replace-state! {:page :c}])
      % := :c
      (reset! !command [::back!])
      % := :a
      (reset! !command [::forward!])
      % := :c
      )))

(p/def encode identity)
(p/def decode identity)

;;; 3. Link

(p/def build-route {})

(p/defn Link [route Body]
  (let [next-route (build-route history route)]
    (dom/a
      (dom/props {::dom/href (encode next-route)})
      (new Body)
      (when-some [e (dom/Event. "click" false)]
        (.preventDefault e)
        (navigate! !history next-route)))))

(defmacro link [route & body]
  `(new Link ~route (p/fn [] ~@body)))


(comment
  (defn ^:no-doc route-cleanup [path m]
    (let [cleanup (fn [m] (when m
                            (not-empty
                              (persistent!
                                (reduce-kv (fn [r k v]
                                             (if (data/nil-or-empty? v)
                                               (dissoc! r k)
                                               r)) (transient m) m)))))]
      (case (count path)
        0 (cleanup m)
        1 (route-cleanup [] (update m (first path) cleanup))
        (route-cleanup (butlast path) (update-in m path cleanup))))))
