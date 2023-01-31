(ns hyperfiddle.photon-ui4
  "uses photon-dom2, so no syntax for text and props"
  #?(:cljs (:require-macros hyperfiddle.photon-ui4))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require
    [contrib.str]
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom2 :as dom]
    [hyperfiddle.photon-dom :as dom1]
    [missionary.core :as m]))

(defmacro handle [getter V!]
  `(p/fn [e#]
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
     (do1 (dom/on "click" (p/fn [e#]
                            (dom/props {:disabled true, :aria-busy true})
                            (new ~V!))) ; do we need to pass the event?
          ~@body)))


;;; TYPEAHEAD

;; TODO nil
(p/defn Latch [impulse init] (p/with-cycle [v init] (if (some? impulse) impulse v)))

#?(:cljs (defn first-option [elem] (-> elem .-parentElement .-firstElementChild)))
#?(:cljs (defn last-option  [elem] (-> elem .-parentElement .-lastElementChild)))
#?(:cljs (defn next-option  [elem] (-> elem .-nextElementSibling (or (first-option elem)))))
#?(:cljs (defn prev-option  [elem] (-> elem .-previousElementSibling (or (last-option elem)))))

#?(:cljs (defn own [event] (.stopPropagation event) (.preventDefault event)))

(defmacro ?mark-selected [selected] `(when (= dom1/node ~selected) (dom/props {:class ["hyperfiddle-selected"]})))

(defmacro return-on-click [return V! id]
  `(dom/on "click" (p/fn [e#] (own e#)
                     (dom/props {:style {:background-color "yellow"}})
                     (~return (p/server (new ~V! ~id))))))

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
                                  (~return (p/server (new ~V! id#))))))
     "Tab"       (if-some [elem# @~!selected]
                   (let [id# (get-id elem#)] (~return (p/server (new ~V! id#))))
                   (~return nil))
     #_else      ::unhandled))

(defmacro for-truncated [[sym opts] limit & body]
  `(let [limit# ~limit, opts# ~opts, truncated# (take limit# opts#), more?# (seq (drop limit# opts#))]
     (p/for [~sym truncated#] ~@body)
     (when more?#
       (p/client
         (dom/div (dom/text "refine your query…")
           (dom/props {:disabled true, :style {:background-color "whitesmoke" :font-size "0.8rem" :font-style "italic"}}))))))

;; TODO
;; - what if the change callback throws
(defmacro typeahead [v V! Options OptionLabel & body]
  `(let [v# ~v, V!# ~V!, Options# ~Options, OptionLabel# ~OptionLabel]
     (p/client
       (dom/div (dom/props {:class "hyperfiddle-typeahead"})
         (let [container-node# dom1/node]
           (do1
             (dom/input
               (let [input-node# dom1/node
                     return# (dom/on "focus"
                               (p/fn [_#]
                                 (set! (.-value dom1/node) "")
                                 (let [return# (missionary.core/dfv)
                                       search# (new Latch (dom/on "input" (p/fn [e#] (value e#))) "")]
                                   (binding [dom1/node container-node#]
                                     (let [!selected# (atom nil), selected# (p/watch !selected#)]
                                       (dom/div (dom/props {:class "hyperfiddle-modal-backdrop"})
                                         (dom/on "click" (p/fn [e#] (return# nil))))
                                       (dom/on "keydown" (p/fn [e#] (handle-meta-keys e# input-node# return# !selected# V!#)))
                                       (dom/ul
                                         (p/server
                                           (for-truncated [id# (new Options# search#)] 20
                                             (p/client
                                               (dom/li (dom/text (p/server (new OptionLabel# id#)))
                                                 (on-mount (swap! !selected# select-if-first dom1/node))
                                                 (on-unmount (swap! !selected# ?pass-on-to-first dom1/node))
                                                 (track-id dom1/node id#)
                                                 (?mark-selected selected#)
                                                 (dom/on "mouseover" (p/fn [e#] (reset! !selected# dom1/node)))
                                                 (return-on-click return# V!# id#))))))))
                                   (new (p/task->cp return#)))))]
                 (case return#
                   (let [txt# (p/server (new OptionLabel# v#))]
                     (case return# (set! (.-value input-node#) txt#))))
                 return#))
             ~@body))))))

(defmacro select [v V! Options OptionLabel & body] ; server biased!
  `(let [v# ~v, V!# ~V!, Options# ~Options, OptionLabel# ~OptionLabel]
     (p/client
       (dom/div (dom/props {:class "hyperfiddle-select"})
         (let [container-node# dom1/node]
           (do1
             (dom/input (dom/props {:style {:caret-color "transparent"}}) ; hides cursor
               (let [input-node# dom1/node
                     return#
                     (dom/on "focus"
                       (p/fn [_#]
                         (let [return#    (missionary.core/dfv)
                               !selected# (atom nil), selected# (p/watch !selected#)]
                           (binding [dom1/node container-node#]
                             (dom/div (dom/props {:class "hyperfiddle-modal-backdrop"})
                               (dom/on "click" (p/fn [e#] (return# nil))))
                             (dom/on "keydown" (p/fn [e#] (case (handle-meta-keys e# input-node# return# !selected# V!#)
                                                            ::unhandled (own e#)
                                                            #_else      nil)))
                             (dom/ul
                               (p/server
                                 (p/for [id# (new Options#)]
                                   (p/client
                                     (let [txt# (p/server (new OptionLabel# id#))]
                                       (dom/li (dom/text txt#)
                                         (when (= txt# (.-value input-node#)) (reset! !selected# dom1/node))
                                         (track-id dom1/node id#)
                                         (?mark-selected selected#)
                                         (dom/on "mouseover" (p/fn [e#] (reset! !selected# dom1/node)))
                                         (return-on-click return# V!# id#))))))))
                           (new (p/task->cp return#)))))]
                 (case return#
                   (let [txt# (p/server (new OptionLabel# v#))]
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
     (p/client
       (dom/div (dom/props {:class "hyperfiddle-tag-picker"})
         (let [container-node# dom1/node]
           (do1
             (dom/ul (dom/props {:class "hyperfiddle-tag-picker-items"})
               (p/server
                 (p/for [id# v#]
                   (let [txt# (new OptionLabel# id#)]
                     (p/client (dom/li (dom/text txt#)
                                 (dom/span (dom/text "×")
                                   (dom/on "click" (p/fn [e#] (own e#) (p/server (new unV!# id#)))))))))))
             (dom/div (dom/props {:class "hyperfiddle-tag-picker-input-container"})
               (let [input-container-node# dom1/node]
                 (dom/input
                   (let [input-node# dom1/node]
                     (binding [dom1/node container-node#] (dom/on "click" (p/fn [e#] (own e#) (focus input-node#))))
                     (dom/on "focus"
                       (p/fn [_#]
                         (let [return# (missionary.core/dfv)
                               search# (new Latch (dom/on "input" (p/fn [e#] (value e#))) "")]
                           (binding [dom1/node input-container-node#]
                             (let [!selected# (atom nil), selected# (p/watch !selected#)]
                               (dom/div (dom/props {:class "hyperfiddle-modal-backdrop"})
                                 (dom/on "click" (p/fn [e#] (own e#) (return# nil))))
                               (dom/on "keydown" (p/fn [e#] (handle-meta-keys e# input-node# return# !selected# V!#)))
                               (dom/ul
                                 (p/server
                                   (for-truncated [id# (new Options# search#)] 20
                                     (p/client
                                       (dom/li (dom/text (p/server (new OptionLabel# id#)))
                                         (on-mount (swap! !selected# select-if-first dom1/node))
                                         (on-unmount (swap! !selected# ?pass-on-to-first dom1/node))
                                         (track-id dom1/node id#)
                                         (?mark-selected selected#)
                                         (dom/on "mouseover" (p/fn [e#] (reset! !selected# dom1/node)))
                                         (return-on-click return# V!# id#))))))))
                           (let [ret# (new (p/task->cp return#))]
                             (case ret# (set! (.-value input-node#) ""))
                             ret#))))))))
             ~@body))))))
