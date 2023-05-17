(ns hyperfiddle.electric-ui4
  #?(:cljs (:require-macros hyperfiddle.electric-ui4))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require
    clojure.edn
    [contrib.str]
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-dom2 :as dom]
    [missionary.core :as m]))

(defmacro do1 [x & body] `(let [ret# ~x] ~@body ret#))

#?(:cljs (defn value [^js e] (.-target.value e))) ; workaround inference warnings, todo rename
#?(:cljs (defn checked [^js e] (.-target.checked e)))

(defmacro control [event-type parse unparse v V! setter & body]
  `(let [[state# v#] (e/for-event-pending-switch [e# (e/listen> dom/node ~event-type)]
                       (some->> (~parse e#) (new ~V!)))]
     (dom/style {:background-color (when (= ::e/pending state#) "yellow")})
     ; workaround "when-true" bug: extra outer when-some added to guard a nil from incorrectly sneaking through
     (when-some [v# (when (and (not (new dom/Focused?)) (#{::e/init ::e/ok} state#)) ~v)]
       (~setter dom/node (~unparse v#))) ; js coerce
     ~@body
     (case state# (::e/pending ::e/failed) (throw v#) (::e/init ::e/ok) v#)))

(defmacro input [v V! & body]
  `(dom/input
     (control "input" value identity ~v ~V! dom/set-val ~@body)))

(defmacro textarea [v V! & body]
  `(dom/textarea
     (control "input" value identity ~v ~V! dom/set-val ~@body)))

(defn parse-edn [s] (try (some-> s contrib.str/blank->nil clojure.edn/read-string) (catch #?(:clj Throwable :cljs :default) _)))
(defn keep-if [pred v] (when (pred v) v))
(defn parse-keyword [s] (keep-if keyword? (parse-edn s)))
(defn parse-symbol [s] (keep-if symbol? (parse-edn s)))
(defn parse-date [s]
  (try #?(:clj (java.time.LocalDate/parse s) :cljs (js/Date. s))
       (catch #?(:clj Throwable :cljs :default) _)))

(defn parse-datetime-local [s]
  (try #?(:clj (java.time.LocalDateTime/parse s) :cljs (js/Date. s))
       (catch #?(:clj Throwable :cljs :default) _)))

(defmacro edn [v V! & body]
  `(dom/textarea
     (control "input" (comp parse-edn value) contrib.str/pprint-str ~v ~V! dom/set-val ~@body)))

(defmacro checkbox [v V! & body]
  `(dom/input (dom/props {:type "checkbox"})
     (control "change" checked identity ~v ~V! #(set! (.-checked %) %2) ~@body)))

(defmacro long [v V! & body]
  `(dom/input (dom/props {:type "number"})
     (control "input" (comp parse-long value) identity ~v ~V! dom/set-val ~@body)))

(defmacro range [v V! & body]
  `(dom/input (dom/props {:type "range"})
     (control "input" (comp parse-long value) identity ~v ~V! dom/set-val ~@body)))

(defmacro double [v V! & body]
  `(dom/input (dom/props {:type "number"})
     (control "input" (comp parse-double value) identity ~v ~V! dom/set-val ~@body)))

(def uuid-pattern "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")
(defmacro uuid [v V! & body]
  `(dom/input (dom/props {:type "text" :pattern uuid-pattern})
     (control "input" (comp parse-uuid value) identity ~v ~V! dom/set-val ~@body)))

(defmacro keyword [v V! & body]
  `(dom/input
     (control "input" (comp parse-keyword value) identity ~v ~V! dom/set-val ~@body)))

(defmacro symbol [v V! & body]
  `(dom/input
     (control "input" (comp parse-symbol value) identity ~v ~V! dom/set-val ~@body)))

(defmacro date [v V! & body]
  `(dom/input (dom/props {:type "date"})
     (control "input" (comp parse-date value) identity ~v ~V! dom/set-val ~@body)))

(defmacro datetime-local [v V! & body]
  `(dom/input (dom/props {:type "datetime-local"})
     (control "input" (comp parse-datetime-local value) identity ~v ~V! dom/set-val ~@body)))

(defmacro button [V! & body] ; "submit-1" vs "submit-N"
  "On click, run possibly remote effect V! and disable the button (preventing 
further clicks) until V! completes (is not pending). Returns the state of the 
button which starts nil and then when clicked becomes the result of V!, which 
can be pending."
  ; This is really a "simple transaction button"
  `(dom/button
     (let [[state# v#] (e/do-event-pending [e# (e/listen> dom/node "click")]
                         (new ~V!))
           busy# (= ::e/pending state#)]
       (dom/style {:border (str "2px solid "
                             (case state# ::e/init "gray" ::e/ok "green" ::e/pending "yellow" ::e/failed "red"))
                   :border-radius "0.2rem"})
       (dom/props {:disabled busy#, :aria-busy busy#}) ; backpressure the user
       ~@body
       (case state# ; 4 colors
         (::e/pending ::e/failed) (throw v#)
         (::e/init ::e/ok) v#))))

#?(:cljs (defn ?read-line! [node e]
           (let [line (.-value node)]
             (when (and (= "Enter" (.-key e)) (contrib.str/empty->nil line))
               (set! (.-value node) "") line))))

(defmacro on-submit
  ([V!] `(on-submit (e/listen> dom/node "keydown" (partial ?read-line! dom/node)) ~V!))
  ([>event V!]
   `(let [[state# v#] (e/for-event-pending [e# ~>event] (new ~V! e#))]
      (case state# (::e/pending ::e/failed) (throw v#) (::e/init ::e/ok) v#))))

;;; TYPEAHEAD

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
         (dom/div (dom/text "refine your query...")
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
                     [state# return#]
                     (e/do-event-pending [e# (e/listen> dom/node "focus")]
                       (set! (.-value dom/node) "")
                       (let [return# (missionary.core/dfv)
                             search# (or (dom/on! "input" value) "")]
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
                                       (swap! !selected# select-if-first dom/node)
                                       (e/on-unmount #(swap! !selected# ?pass-on-to-first dom/node))
                                       (track-id dom/node id#)
                                       (?mark-selected selected#)
                                       (dom/on "mouseover" (e/fn [e#] (reset! !selected# dom/node)))
                                       (return-on-click return# V!# id#))))))))
                         (new (e/task->cp return#))))]
                 (case state#
                   (::e/failed ::e/pending) (throw return#)
                   (::e/init ::e/ok) (let [txt# (e/server (new OptionLabel# v#))]
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
                     [state# return#]
                     (e/do-event-pending [e# (e/listen> dom/node "focus")]
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
                         (new (e/task->cp return#))))]
                 (case state#
                   (::e/pending ::e/failed) (throw return#)
                   (::e/init ::e/ok) (let [txt# (e/server (new OptionLabel# v#))]
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
                       (let [[state# ret#]
                             (e/do-event-pending [e# (e/listen> dom/node "focus")]
                               (let [return# (missionary.core/dfv)
                                     search# (or (dom/on! "input" value) "")]
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
                                               (swap! !selected# select-if-first dom/node)
                                               (e/on-unmount #(swap! !selected# ?pass-on-to-first dom/node))
                                               (track-id dom/node id#)
                                               (?mark-selected selected#)
                                               (dom/on "mouseover" (e/fn [e#] (reset! !selected# dom/node)))
                                               (return-on-click return# V!# id#))))))))
                                 (let [ret# (new (e/task->cp return#))]
                                   (case ret# (set! (.-value input-node#) ""))
                                   ret#)))]
                         (case state# (::e/pending ::e/failed) (throw ret#) nil)))))))
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
