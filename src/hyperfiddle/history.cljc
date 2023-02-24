(ns hyperfiddle.history
  (:require [contrib.cljs-target :refer [do-browser]]
            [hyperfiddle.rcf :as rcf :refer [tests % tap with]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m])
  #?(:clj (:import [clojure.lang IRef IAtom]))
  #?(:cljs (:require-macros hyperfiddle.history)))

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


;; Based on a reflexion of the use of cursors in Electric, I’m challengineg goal 1. because:
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
;; deduping would be lazy (handled by Electric)
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
   (defrecord AtomHistory [^IAtom state watches max-size]
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
  (replace-state! [this new-state] (reset! this new-state)))


(defn atom-history
  "Return a new IHistory instance backed by an atom.
  Initial history state can be provided with `initial-state`.
  Default history size is unbounded and can be constrained to `max-size` elements in a FIFO way.
  A negative value or 0 has no effect."
  ([] (atom-history nil 0))
  ([initial-state] (atom-history initial-state 0))
  ([initial-state max-size] (->AtomHistory (atom [[initial-state] 0]) (atom {}) max-size)))

#?(:clj
   (defrecord ProxyHistory [parent ^IAtom state]
     IAtom
     (swap [this f]           (swap! state f))
     (swap [this f arg]       (swap! state f arg))
     (swap [this f arg1 arg2] (swap! state f arg1 arg2))
     (swap [this f x y args]  (apply swap! state f x y args))
     (reset [this newval]     (reset! state newval))
     (compareAndSet [this oldv newv] (compare-and-set! state oldv newv))

     IRef
     (setValidator [_ _] (throw (UnsupportedOperationException. "History does not support validators")))
     (getValidator [_] (throw (UnsupportedOperationException. "History does not support validators")))
     (getWatches [_] (.getWatches state))
     (addWatch [this key callback] (add-watch state key callback) this)
     (removeWatch [_ key] (remove-watch state key))
     (deref [_] (deref state))
     ))


#?(:clj (defmethod print-method ProxyHistory [x w] (print-dup x w)))

#?(:cljs
   (defrecord ProxyHistory [^IHistory parent ^IAtom state]
     IAtom
     ISwap
     (-swap! [this f]           (swap! state f))
     (-swap! [this f arg]       (swap! state f arg))
     (-swap! [this f arg1 arg2] (swap! state f arg1 arg2))
     (-swap! [this f x y args]  (apply swap! state f x y args))

     IReset
     (-reset! [this newval]     (reset! state newval))

     IWatchable
     (-add-watch [this key callback] (add-watch state key callback)
       this)
     (-remove-watch [_ key] (remove-watch state key))

     IDeref
     (-deref [_] (deref state))
     ))

(extend-type ProxyHistory
  IHistory
  (navigate! [this route] (navigate! (.-parent this) route))
  (back! [this] (back! (.-parent this)))
  (forward! [this] (forward! (.-parent this)))
  (replace-state! [this new-state] (reset! this new-state)))


(defn proxy-history
  "Return a new IHistory instance backed by an atom.
  History state is stored in an atom.
  Navigation is forwarded to the `parent` history.
  Initial state is provided with `initial-state`. "
  ([parent] (proxy-history parent nil))
  ([parent initial-state] (->ProxyHistory parent (atom initial-state)))) ; keep state local, not in url


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

(e/def !history nil)                    ; History instance mutable ref
(e/def history ::unset)
(e/def route ::unset)
(e/def path [])
(e/def swap-route! nil)

(defn update-in* [m path f & args]
  (if (empty? path)
    (apply f m args)
    (apply update-in m path f args)))

(defn check-route! [route]
  (assert (or (nil? route) (associative? route)) (str "A route should be an associative data structure. Given " route)))

(defn default-platform-history []
  #?(:clj (atom-history)
     :cljs (atom-history)))

(defn cleanup-on-unmount [h path]
  (cond (empty? path)      h
        (= 1 (count path)) (dissoc h (first path))
        :else              (update-in* h (butlast path) dissoc (last path))))

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
  `(let [history-or-ident#          ~history-or-ident
         ident#                     (if (or (ident? history-or-ident#)
                                            (integer? history-or-ident#))
                                      history-or-ident# nil)
         path#                      (if (nil? ident#) [] [ident#])
         [rebound?# history# path#] (if (or (ident? history-or-ident#)
                                            (integer? history-or-ident#)) ; if we focus on a sub route
                                      [false !history (into path path#)] ; same history, different path
                                      (let [h# (or history-or-ident# (default-platform-history))]
                                        (if (= h# !history) ; if we rebind to the same history
                                          [false h# path#]  ; noop
                                          [true h# []])     ; new history, root path
                                        ))]
     (binding [!history history#
               path     path#]
       (binding [history (if (or rebound?# (= ::unset history)) (e/watch !history) history)]
         (binding [route (let [route (if (or rebound?# (= ::unset route)) history route)]
                           (check-route! route)
                           (if (some? ident#)
                             (get route ident#)
                             route))]
           (binding [swap-route! (partial (fn [!history# path# & args#]
                                            (swap! !history# (fn [r#] (apply update-in* r# path# args#))))
                                   !history path)]
             (e/on-unmount #(swap! !history cleanup-on-unmount path#))
             ~@body))))))

;::render-title (set! (.-title js/document) (str (clojure.string/capitalize (name (first route))) " - Hyperfiddle"))

(tests
  (with (e/run (try (router nil (tap route))
                    (catch Throwable t
                      (prn t))))
    % := nil))

(tests
  (with (e/run (router (atom-history {:a "hello"})
                 (tap route)
                 (router :a
                   (tap route))))
    % := {:a "hello"}
    % := "hello"))

(tests
  (with (e/run (router nil
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
    (with (e/run (binding [!history (atom-history)]
                   (router :page
                     (tap route)
                     (let [[command value] (e/watch !command)]
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

(e/def encode identity) ; turn sexpr to string href - ednish encoding, hopefully simple
(e/def decode identity)

;;; 3. Link

(e/def build-route {})

(e/defn Link [route Body]
  (let [next-route (build-route history route)]
    (dom/a
      (dom/props {::dom/href (encode next-route)})
      (new Body)
      (when-some [e (dom/Event. "click" false)]
        (.preventDefault e)
        (navigate! !history next-route)))))

(defmacro link [route & body]
  ; all links are Sexpr-like and head is qualified name. like [::secrets 1 2]
  `(new Link ~route (e/fn [] ~@body)))


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


;; HTML5 integration

#?(:cljs
   (do-browser

     (defn throttler [rate-ms]
       (let [!nextf (atom nil)
             !running (atom false)]
         (fn rec [f]
           (if @!running
             (reset! !nextf f)
             (do (reset! !running true)
                 (f)
                 (.setTimeout js/window (fn [] (reset! !running false)
                                          (when-let [nextf @!nextf]
                                            (reset! !nextf nil)
                                            (rec nextf)))
                   rate-ms))))))

     ;; User agent limits HistoryAPI to 100 changes / 30s timeframe (https://bugs.webkit.org/show_bug.cgi?id=156115)
     ;; Firefox and Safari log an error and ignore further HistoryAPI calls for security reasons.
     ;; Chrome does the same but can also hang the tab: https://bugs.chromium.org/p/chromium/issues/detail?id=1038223
     (let [throttle (throttler 300)]          ; max 3changes/s, 90/30s
       (defn replaceState! [path] (throttle #(.replaceState js/window.history nil "" path))))

     (defn html5-pushState! [path] (.pushState js/window.history nil "" path))
     (defn html5-back! [] (.back js/window.history))
     (defn html5-forward! [] (.forward js/window.history))

     (def html5-location (constantly (.-location js/window)))

     (defn html5-path []
       (let [loc (html5-location)]
         (str (.-pathname loc) (.-search loc) (.-hash loc))))

     (defrecord HTML5History [encode decode !state]
       IAtom
       ISwap
       (-swap! [this f] (let [[_oldval newval] (swap-vals! !state f)]
                          (replaceState! (encode newval))
                          newval))
       (-swap! [this f arg] (-swap! this #(f % arg)))
       (-swap! [this f arg1 arg2] (-swap! this #(f % arg1 arg2)))
       (-swap! [this f x y args] (-swap! this #(apply f % x y args)))

       IReset
       (-reset! [this newval] (-swap! this (constantly newval)))

       IWatchable
       (-add-watch [this key callback]
         (add-watch !state key callback)
         this)
       (-remove-watch [_ key] (remove-watch !state key))

       IDeref
       (-deref [_] @!state)

       IHistory
       (navigate! [this route]
         (html5-pushState! (encode route))
         (reset! (.-!state this) route))
       (back! [^HTML5History this]
         (html5-back!)
         (reset! (.-!state this) (decode (html5-path))))
       (forward! [^HTML5History this]
         (html5-forward!)
         (reset! (.-!state this) (decode (html5-path))))
       (replace-state! [this new-state]
         (reset! this new-state))

       ;; TODO Implement equality so that two HTML5History are always equal. The
       ;;      browser history is a singleton. There is no use case for nested
       ;;      HTML5History instances on the page.
       )

     (defn html5-history [encode decode] (->HTML5History encode decode (atom (decode (html5-path)))))


     (defn -html5-history-get-state [^HTML5History this] (.-!state this))

     (e/defn HTML5-History [] ; TODO make this flow a singleton (leverage m/signal in next reactor iteration)
       (let [history (html5-history encode decode)
             decode' decode]
         (new (m/observe (fn [!]
                           (! nil)
                           (let [f (fn [_e] (reset! (-html5-history-get-state history) (decode' (html5-path))))]
                             (.addEventListener js/window "popstate" f)
                             #(.removeEventListener js/window "popstate" f)))))
         history))

     ))