(ns hyperfiddle.photon-ui4
  "uses photon-dom2, so no syntax for text and props"
  #?(:cljs (:require-macros hyperfiddle.photon-ui4))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require
    [contrib.str]
    [clojure.string :as str]
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom2 :as dom]
    [hyperfiddle.photon-dom :as dom1]
    [hyperfiddle.photon-ui2 :as ui2 :refer [parse-edn parse-keyword parse-symbol parse-date]]))

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

(defmacro date [v V! & body]
  `(dom/input {:type "date"}
     (dom/bind-value ~v)
     (do1 (dom/on "input" (handle (comp parse-date value) ~V!)) ~@body)))

(defmacro button [V! & body]
  `(dom/button
     (do1 (dom/on "click" (p/fn [e#]
                            (dom/props {:disabled true, :aria-busy true})
                            (new ~V!))) ; do we need to pass the event?
          ~@body)))


;;; TYPEAHEAD

;; TODO nil
(p/defn Latch [impulse init] (p/with-cycle [v init] (if (some? impulse) impulse v)))

;; TODO more robust click-outside handling
(p/defn CloseOnClickUnlessInput [return input-node]
  (binding [dom1/node js/document] ; breaks if stopPropagation is called by non-local component
    (dom/on "click" (p/fn [e] (when (not= input-node (.-target e)) (return nil))))))

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
                                 (let [return# (missionary.core/dfv)
                                       search# (new Latch (dom/on "input" (p/fn [e#] (.-target.value e#)))
                                                 (.-value input-node#))]
                                   (new CloseOnClickUnlessInput return# input-node#)
                                   (binding [dom1/node container-node#]
                                     (dom/ul
                                       (p/server
                                         (p/for [id# (new Options# search#)]
                                           (p/client
                                             (dom/div (dom/text (p/server (new OptionLabel# id#)))
                                               (dom/on "click" (p/fn [evt#]
                                                                 (dom/props {:style {:background-color "yellow"}})
                                                                 (.preventDefault evt#) (.stopPropagation evt#)
                                                                 (return# (p/server (new V!# id#)))))))))))
                                   (new (p/task->cp return#)))))]
                 (case return#
                   (let [txt# (p/server (new OptionLabel# v#))]
                     (set! (.-value input-node#) txt#)))
                 return#))
             ~@body))))))

;; TODO
;; - keyboard
;; - what if the change callback throws

;; tests
#?(:clj (def -data {:alice "Alice B", :bob "Bob C", :charlie "Charlie D", :derek "Derek B"}))
#?(:clj (defn q [search] (into [] (comp (filter #(or (empty? search) (str/includes? (second %) search))) (map first)) -data)))

(p/defn TypeaheadDemo []
  (p/server
    (let [!v (atom :alice)]
      (typeahead (p/watch !v) (p/fn [id] (prn [:picked id]) (reset! !v id))
        (p/fn [search] (prn [:options search]) (q search))
        (p/fn [e] (prn [:render e]) (get -data e))
        ;; body
        ))))
