(ns hyperfiddle.history2
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [contrib.cljs-target :refer [do-browser]]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.rcf :as rcf :refer [tests % tap with]]
   [missionary.core :as m]
   )
  #?(:clj (:import [clojure.lang IRef IAtom]))
  #?(:cljs (:require-macros hyperfiddle.history2))
  )

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

(e/def history nil)                    ; History instance mutable ref

;; HTML5 integration

(defn absolute [path]
  (assert (string? path))
  (str "/" (str/replace-first path #"^/+" "")))

(tests
  (absolute "foo")        := "/foo"
  (absolute "/foo")       := "/foo"
  (absolute "//foo")      := "/foo"
  (absolute "//foo//bar") := "/foo//bar")

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
     (let [throttle (throttler 300)]    ; max 3changes/s, 90/30s
       (defn replaceState! [path] (throttle #(.replaceState js/window.history (.. js/window -history -state) "" (absolute path)))))

     ;; History entries' ids must be unique across page refreshes.
     ;; - A monotonic counter would reset and ids would collide after a refresh.
     ;; - Date.now() is not precise enough, two programatic navigations could happen at the same millisecond.
     ;; - We use a mix of both: monotonic counter seeded by current time * 100.
     ;;   - Supports 100 navigations per millisecond.
     ;;   - doesn't collide after a page refresh.
     (def history-entry-uid (partial swap! (atom (* (.now js/Date) 100)) inc))

     (defn html5-pushState! [next-position path] (.pushState js/window.history #js{:position next-position} "" (absolute path)))
     (defn html5-back! [] (.back js/window.history))
     (defn html5-forward! [] (.forward js/window.history))

     (defn html5-path []
       (let [loc (.-location js/window)]
         (str (.-pathname loc) (.-search loc) (.-hash loc))))

     (defn- index-of [xs val]
       (loop [i 0
              [x & xs] xs]
         (cond (= x val) i
               (seq xs) (recur (inc i) xs)
               :else -1)))

     (defrecord HTML5History [!state !stack !position]
       IAtom
       ISwap
       (-swap! [this f] (let [[_oldval newval] (swap-vals! !state f)]
                          (replaceState! newval)
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
         (let [prev-position @!position
               next-position (history-entry-uid)]
           (swap! !stack (fn [stack]
                           (let [index (index-of stack prev-position)]
                             (case index
                               -1 (conj stack next-position)
                               (conj (subvec stack 0 (inc index)) next-position)))))
           (reset! !position next-position)
           (html5-pushState! next-position route))
         (reset! (.-!state this) route))
       (back! [^HTML5History this]
         (html5-back!)
         (swap! !position (fn [pos stack]
                            (let [index (index-of stack pos)]
                              (if (= 0 index) pos (get stack (dec index)))))
           @!stack)
         (reset! (.-!state this) (html5-path)))
       (forward! [^HTML5History this]
         (html5-forward!)
         (swap! !position (fn [pos stack]
                            (let [index (index-of stack pos)]
                              (if (= index (dec (count stack))) pos (get stack (inc index)))))
           @!stack)
         (reset! (.-!state this) (html5-path)))
       (replace-state! [this new-state]
         (reset! this new-state))

       ;; TODO Implement equality so that two HTML5History are always equal. The
       ;;      browser history is a singleton. There is no use case for nested
       ;;      HTML5History instances on the page.
       )

     (defn html5-history []
       ;; Browser History API forbids to prevent navigation. Use case: prompt user for confirmation of unsaved data loss.
       ;; We must implement it ourselves by reverting the navigation action. However:
       ;;  - navigation direction (back/forward) is not provided.
       ;;  - current position in history is not provided.
       ;; To mitigate, we keep a stack of navigated pages and persist it to
       ;; SessionStorage, so it survives refreshes.

       ;; "popstate" events will contain the current page position, the history
       ;; instance will contain the previous one. Given the stack, the previous
       ;; position and current position, we compute the distance (delta) between the two
       ;; pages (one can jump over multiple pages at once through history).
       ;; We then call History.go(-delta) to revert the navigation.
       ;; See `HTML5-Navigation-Intents`.
       (let [position  (or (when-let [state (.. js/window -history -state)]
                             (.-position state))
                         (history-entry-uid))
             stack     (if-let [array (.. js/window -sessionStorage (getItem "hyperfiddle_history_stack"))]
                         (edn/read-string array)
                         [position])
             !position (atom position)
             !stack    (atom stack)]
         (.replaceState (.-history js/window) #js{:position position} nil)
         (add-watch !stack ::stack (fn [_ _ _ stack] (.. js/window -sessionStorage (setItem "hyperfiddle_history_stack" (pr-str stack)))))
         (->HTML5History (atom (html5-path)) !stack !position)))


     (defn -html5-history-get-state [^HTML5History this] (.-!state this))

     (defn nav-delta [stack prev-position curr-position]
       (- (index-of stack curr-position) (index-of stack prev-position)))

     ))







