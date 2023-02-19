(ns hyperfiddle.router-html5
  (:require [hyperfiddle.history :as router]
            [hyperfiddle.electric :as e]
            [missionary.core :as m]))

(defn throttler [rate-ms]
  (let [!nextf   (atom nil)
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

(defn pushState!    [path] (.pushState    js/window.history nil "" path))
(defn back!         []     (.back         js/window.history))
(defn forward!      []     (.forward      js/window.history))

(def location (constantly (.-location js/window)))

(defn path [] (let [loc (location)]
                (str (.-pathname loc) (.-search loc) (.-hash loc))))

(defrecord HTML5History [encode decode !state]
  IAtom
  ISwap
  (-swap! [this f]           (let [[_oldval newval] (swap-vals! !state f)]
                               (replaceState! (encode newval))
                               newval))
  (-swap! [this f arg]       (-swap! this #(f % arg)))
  (-swap! [this f arg1 arg2] (-swap! this #(f % arg1 arg2)))
  (-swap! [this f x y args]  (-swap! this #(apply f % x y args)))

  IReset
  (-reset! [this newval]     (-swap! this (constantly newval)))

  IWatchable
  (-add-watch [this key callback]
    (add-watch !state key callback)
    this)
  (-remove-watch [_ key] (remove-watch !state key))

  IDeref
  (-deref [_] @!state)

  router/IHistory
  (navigate! [this route]
    (pushState! (encode route))
    (reset! (.-!state this) route))
  (back! [^HTML5History this]
    (back!)
    (reset! (.-!state this) (decode (path))))
  (forward! [^HTML5History this]
    (forward!)
    (reset! (.-!state this) (decode (path))))
  (replace-state! [this new-state]
    (reset! this new-state))

  ;; TODO Implement equality so that two HTML5History are always equal. The
  ;;      browser history is a singleton. There is no use case for nested
  ;;      HTML5History instances on the page.
  )

(defn html5-history [encode decode] (->HTML5History encode decode (atom (decode (path)))))


(defn -get-state [^HTML5History this] (.-!state this))

(e/defn HTML5-History [] ; TODO make this flow a singleton (leverage m/signal in next reactor iteration)
  (let [history (html5-history router/encode router/decode)
        decode  router/decode]
    (new (m/observe (fn [!]
                      (! nil)
                      (let [f (fn [_e] (reset! (-get-state history) (decode (path))))]
                        (.addEventListener js/window "popstate" f)
                        #(.removeEventListener js/window "popstate" f)))))
    history))
