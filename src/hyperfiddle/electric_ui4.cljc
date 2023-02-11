(ns hyperfiddle.electric-ui4
  #?(:cljs (:require-macros hyperfiddle.electric-ui4))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require
    clojure.edn
    [contrib.str]
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-dom2 :as dom]
    [missionary.core :as m]))

(defmacro handle [getter V!]
  `(e/fn [e#]
     (dom/props {:style {:background-color "yellow"}})
     (when-some [v# (~getter e#)] (new ~V! v#))))

(defmacro do1 [x & body] `(let [ret# ~x] ~@body ret#))

#?(:cljs (defn value [^js e] (.-target.value e)))
#?(:cljs (defn checked [^js e] (.-target.checked e)))

(defmacro input [v V! & body]           ; todo nominal args
  `(dom/input
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle value ~V!)) ~@body)))

(defmacro textarea [v V! & body]
  `(dom/textarea
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle value ~V!)) ~@body)))

(defn parse-edn [s] (try (some-> s contrib.str/blank->nil clojure.edn/read-string) (catch #?(:clj Throwable :cljs :default) _)))
(defn keep-if [pred v] (when (pred v) v))
(defn parse-keyword [s] (keep-if keyword? (parse-edn s)))
(defn parse-symbol [s] (keep-if symbol? (parse-edn s)))

(defmacro edn [v V! & body]
  `(dom/textarea
     (dom/bind-value (contrib.str/pprint-str ~v))
     (do1 (dom/on "input" (handle (comp parse-edn value) ~V!)) ~@body)))

(defmacro checkbox [v V! & body]
  `(dom/input (dom/props {:type "checkbox"})
     (dom/bind-value ~v (fn [node# v#] (set! (.-checked node#) v#)))
     (do1 (dom/on "change" (handle checked ~V!)) ~@body)))

(defmacro long [v V! & body]
  `(dom/input (dom/props {:type "number"})
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle (comp parse-long value) ~V!)) ~@body)))

(defmacro range [v V! & body]
  `(dom/input (dom/props {:type "range"})
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle (comp parse-long value) ~V!)) ~@body)))

(defmacro double [v V! & body]
  `(dom/input (dom/props {:type "number"})
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle (comp parse-double value) ~V!)) ~@body)))

(def uuid-pattern "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")
(defmacro uuid [v V! & body]
  `(dom/input (dom/props {:type "text" :pattern uuid-pattern})
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle (comp parse-uuid value) ~V!)) ~@body)))

(defmacro keyword [v V! & body]
  `(dom/input
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle (comp parse-keyword value) ~V!)) ~@body)))

(defmacro symbol [v V! & body]
  `(dom/input
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle (comp parse-symbol value) ~V!)) ~@body)))

(defn parse-date [s]
  (try #?(:clj (java.time.LocalDate/parse s) :cljs (js/Date. s))
       (catch #?(:clj Throwable :cljs :default) _)))

(defn parse-datetime-local [s]
  (try #?(:clj (java.time.LocalDateTime/parse s) :cljs (js/Date. s))
       (catch #?(:clj Throwable :cljs :default) _)))

(defmacro date [v V! & body]
  `(dom/input (dom/props {:type "date"})
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle (comp parse-date value) ~V!)) ~@body)))

(defmacro datetime-local [v V! & body]
  `(dom/input (dom/props {:type "datetime-local"})
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle (comp parse-datetime-local value) ~V!)) ~@body)))

(defmacro button [V! & body]
  `(dom/button
     (do1 (dom/on "click" (e/fn [e#]
                            (dom/props {:disabled true, :aria-busy true})
                            (new ~V!))) ; do we need to pass the event?
          ~@body)))


;;; TYPEAHEAD

;; TODO nil
(e/defn Latch [impulse init] (e/with-cycle [v init] (if (some? impulse) impulse v)))

#?(:cljs (defn first-option [elem] (-> elem .-parentElement .-firstElementChild)))
#?(:cljs (defn last-option  [elem] (-> elem .-parentElement .-lastElementChild)))
#?(:cljs (defn next-option  [elem] (-> elem .-nextElementSibling (or (first-option elem)))))
#?(:cljs (defn prev-option  [elem] (-> elem .-previousElementSibling (or (last-option elem)))))

#?(:cljs (defn own [event] (.stopPropagation event) (.preventDefault event)))

(defmacro ?mark-selected [selected] `(when (= dom/node ~selected) (dom/props {:class ["hyperfiddle-selected"]})))

(defmacro return-on-click [return V! id]
  `(dom/on "click" (e/fn [e#] (own e#)
                     (dom/props {:style {:background-color "yellow"}})
                     (~return (e/server (new ~V! ~id))))))

(defmacro on-mount [& body] `(new (m/observe #(do (% nil) ~@body (fn [])))))
(defmacro on-unmount [& body] `(new (m/observe #(do (% nil) (fn [] ~@body)))))

#?(:cljs (defn ?pass-on-to-first [selected elem]
           (if (= selected elem)
             (let [fst (first-option elem)]
               (if (= fst elem)
                 (-> elem .-parentElement .-nextElementSibling)
                 fst))
             selected)))

#?(:cljs (defn select-if-first [selected elem] (if (= elem (first-option elem)) elem selected)))

#?(:cljs (defn track-id [node id] (set! (.-hyperfiddle-id ^js node) id)))
#?(:cljs (defn get-id [node] (.-hyperfiddle-id ^js node)))

(defmacro handle-meta-keys [e input-node return !selected V!]
  `(case (.-key ~e)
     "Escape"    (do (own ~e) (.blur ~input-node) (~return nil))
     "ArrowDown" (do (own ~e) (swap! ~!selected next-option))
     "ArrowUp"   (do (own ~e) (swap! ~!selected prev-option))
     "Enter"     (do (own ~e) (when-some [elem# @~!selected]
                                (let [id# (get-id elem#)]
                                  (.blur ~input-node)
                                  (~return (e/server (new ~V! id#))))))
     "Tab"       (if-some [elem# @~!selected]
                   (let [id# (get-id elem#)] (~return (e/server (new ~V! id#))))
                   (~return nil))
     #_else      ::unhandled))

(defmacro for-truncated [[sym opts] limit & body]
  `(let [limit# ~limit, opts# ~opts, truncated# (take limit# opts#), more?# (seq (drop limit# opts#))]
     (e/for [~sym truncated#] ~@body)
     (when more?#
       (e/client
         (dom/div (dom/text "refine your query…")
           (dom/props {:disabled true, :style {:background-color "whitesmoke" :font-size "0.8rem" :font-style "italic"}}))))))

;; TODO
;; - what if the change callback throws
(defmacro typeahead [v V! Options OptionLabel & body] ; server biased
  `(let [v# ~v, V!# ~V!, Options# ~Options, OptionLabel# ~OptionLabel]
     (e/client
       (dom/div (dom/props {:class "hyperfiddle-typeahead"})
         (let [container-node# dom/node]
           (do1
             (dom/input
               (let [input-node# dom/node
                     return# (dom/on "focus"
                               (e/fn [_#] ; FIXME Exceptions seems to be swallowed here
                                 (set! (.-value dom/node) "")
                                 (let [return# (missionary.core/dfv)
                                       search# (new Latch (dom/on "input" (e/fn [e#] (value e#))) "")]
                                   (binding [dom/node container-node#]
                                     (let [!selected# (atom nil), selected# (e/watch !selected#)]
                                       (dom/div (dom/props {:class "hyperfiddle-modal-backdrop"})
                                         (dom/on "click" (e/fn [e#] (return# nil))))
                                       (dom/on "keydown" (e/fn [e#] (handle-meta-keys e# input-node# return# !selected# V!#)))
                                       (dom/ul
                                         (e/server
                                           (for-truncated [id# (new Options# search#)] 20
                                             (e/client
                                               (dom/li (dom/text (e/server (new OptionLabel# id#)))
                                                 (on-mount (swap! !selected# select-if-first dom/node))
                                                 (on-unmount (swap! !selected# ?pass-on-to-first dom/node))
                                                 (track-id dom/node id#)
                                                 (?mark-selected selected#)
                                                 (dom/on "mouseover" (e/fn [e#] (reset! !selected# dom/node)))
                                                 (return-on-click return# V!# id#))))))))
                                   (new (e/task->cp return#)))))]
                 (case return#
                   (let [txt# (e/server (new OptionLabel# v#))]
                     (case return# (set! (.-value input-node#) txt#))))
                 return#))
             ~@body))))))

(defmacro select [v V! Options OptionLabel & body] ; server biased!
  `(let [v# ~v, V!# ~V!, Options# ~Options, OptionLabel# ~OptionLabel]
     (e/client
       (dom/div (dom/props {:class "hyperfiddle-select"})
         (let [container-node# dom/node]
           (do1
             (dom/input (dom/props {:style {:caret-color "transparent"}}) ; hides cursor
               (let [input-node# dom/node
                     return#
                     (dom/on "focus"
                       (e/fn [_#]
                         (let [return#    (missionary.core/dfv)
                               !selected# (atom nil), selected# (e/watch !selected#)]
                           (binding [dom/node container-node#]
                             (dom/div (dom/props {:class "hyperfiddle-modal-backdrop"})
                               (dom/on "click" (e/fn [e#] (return# nil))))
                             (dom/on "keydown" (e/fn [e#] (case (handle-meta-keys e# input-node# return# !selected# V!#)
                                                            ::unhandled (own e#)
                                                            #_else      nil)))
                             (dom/ul
                               (e/server
                                 (e/for [id# (new Options#)]
                                   (e/client
                                     (let [txt# (e/server (new OptionLabel# id#))]
                                       (dom/li (dom/text txt#)
                                         (when (= txt# (.-value input-node#)) (reset! !selected# dom/node))
                                         (track-id dom/node id#)
                                         (?mark-selected selected#)
                                         (dom/on "mouseover" (e/fn [e#] (reset! !selected# dom/node)))
                                         (return-on-click return# V!# id#))))))))
                           (new (e/task->cp return#)))))]
                 (case return#
                   (let [txt# (e/server (new OptionLabel# v#))]
                     (case return# (set! (.-value input-node#) txt#))))
                 return#))
             ~@body))))))

#?(:cljs
   (defn focus [elem]
     (.focus elem #js {"focusVisible" true})
     (.setTimeout js/window #(do
                               (.dispatchEvent elem (js/FocusEvent. "focus"))
                               (.dispatchEvent elem (js/FocusEvent. "focusin" #js {:bubbles true}))))))

(defmacro tag-picker [v V! unV! Options OptionLabel & body]
  `(let [v# ~v, V!# ~V!, unV!# ~unV!, Options# ~Options, OptionLabel# ~OptionLabel]
     (e/client
       (dom/div (dom/props {:class "hyperfiddle-tag-picker"})
         (let [container-node# dom/node]
           (do1
             (dom/ul (dom/props {:class "hyperfiddle-tag-picker-items"})
               (e/server
                 (e/for [id# v#]
                   (let [txt# (new OptionLabel# id#)]
                     (e/client (dom/li (dom/text txt#)
                                 (dom/span (dom/text "×")
                                   (dom/on "click" (e/fn [e#] (own e#) (e/server (new unV!# id#)))))))))))
             (dom/div (dom/props {:class "hyperfiddle-tag-picker-input-container"})
               (let [input-container-node# dom/node]
                 (dom/input
                   (let [input-node# dom/node]
                     (binding [dom/node container-node#] (dom/on "click" (e/fn [e#] (own e#) (focus input-node#))))
                     (if (e/server (nil? V!#))
                       (dom/props {:disabled true})
                       (dom/on "focus"
                         (e/fn [_#]
                           (let [return# (missionary.core/dfv)
                                 search# (new Latch (dom/on "input" (e/fn [e#] (value e#))) "")]
                             (binding [dom/node input-container-node#]
                               (let [!selected# (atom nil), selected# (e/watch !selected#)]
                                 (dom/div (dom/props {:class "hyperfiddle-modal-backdrop"})
                                   (dom/on "click" (e/fn [e#] (own e#) (return# nil))))
                                 (dom/on "keydown" (e/fn [e#] (handle-meta-keys e# input-node# return# !selected# V!#)))
                                 (dom/ul
                                   (e/server
                                     (for-truncated [id# (new Options# search#)] 20
                                       (e/client
                                         (dom/li (dom/text (e/server (new OptionLabel# id#)))
                                           (on-mount (swap! !selected# select-if-first dom/node))
                                           (on-unmount (swap! !selected# ?pass-on-to-first dom/node))
                                           (track-id dom/node id#)
                                           (?mark-selected selected#)
                                           (dom/on "mouseover" (e/fn [e#] (reset! !selected# dom/node)))
                                           (return-on-click return# V!# id#))))))))
                             (let [ret# (new (e/task->cp return#))]
                               (case ret# (set! (.-value input-node#) ""))
                               ret#)))))))))
             ~@body))))))

#?(:cljs (defn sample-scroll-state! [scrollable]
           [(.. scrollable -scrollTop) ; optimization - detect changes (pointless)
            (.. scrollable -scrollHeight) ; snapshot height to detect layout shifts in flipped mode
            (.. scrollable -clientHeight)])) ; measured viewport height (scrollbar length)

#?(:cljs (defn scroll-state> [scrollable]
           (m/observe
             (fn [!]
               (let [sample (fn [] (! (sample-scroll-state! scrollable)))]
                 (.addEventListener scrollable "scroll" sample #js {"passive" true})
                 #(.removeEventListener scrollable "scroll" sample))))))

#?(:cljs (def !scrollStateDebug (atom nil)))

#?(:cljs (defn scroll-state< [scrollable]
           (->> (scroll-state> scrollable)
                (e/throttle 16) ; RAF interval
                (m/reductions {} [0 0 0])
                (m/relieve {})
                (m/latest (fn [[scrollTop scrollHeight clientHeight :as s]]
                            (reset! !scrollStateDebug {::scrollTop scrollTop
                                                       ::scrollHeight scrollHeight
                                                       ::clientHeight clientHeight})
                            s)))))
