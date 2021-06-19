(ns hyperfiddle.client.ui
  (:refer-clojure :exclude [meta time])
  (:require [clojure.set :as set]
            [hfdl.impl.switch :refer [switch]]
            [hfdl.lang :refer [#?@(:clj [defnode vars])]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj [clojure.test :as t])
            #?(:cljs [clojure.edn :as edn])
            #?(:cljs [cljs.test :as t])
            #?(:cljs [hyperfiddle.client.router :as router])
            #?(:cljs [goog.dom :as dom])
            #?(:cljs [goog.dom.xml :as xml])
            #?(:cljs [goog.style :as sty])
            #?(:cljs [goog.events :as events])
            [clojure.string :as str]
            [taoensso.timbre :as log])
  #?(:cljs (:require-macros [hfdl.lang :refer [defnode vars]])))

;; TODO belongs here?
(def change-route! #?(:cljs (comp router/set-route! edn/read-string)))

;; -----------------------------------------------------------------------

(defn create-text-node [initial-value] #?(:cljs (.createTextNode js/document (str initial-value))))
(defn create-tag-node [tag] #?(:cljs (if (= tag :<>)
                                       (.createDocumentFragment js/document)
                                       (.createElement js/document (name tag)))))
(defn by-id [id] #?(:cljs (js/document.getElementById id)))
(defn set-text-content! [elem text] #?(:cljs (set! (.-textContent elem) text)))

(defnode text [x]
  (doto (create-text-node "")
    (set-text-content! x)))

(defn append-childs [parent items] (reduce #?(:cljs #(doto %1 (.appendChild %2))) parent items))
(defn remove-childs [parent items] (reduce #?(:cljs #(doto %1 (.removeChild %2))) parent items))

(defn mount* [parent items]
  (m/observe
   (fn [!]
     (! (append-childs parent items))
     (fn []
       (remove-childs parent items)))))

(defnode mount [parent items]
  ~(mount* parent items))

(defonce ^:const HF-SHADOW-PROPS (str (gensym "hf")))

(defn shadow-props [elem]
  (aget elem HF-SHADOW-PROPS))

(defn set-shadow-props! [elem props] (aset elem HF-SHADOW-PROPS props))

(defn set-style! [elem styles]
  (let [old-props (shadow-props elem)
        old-style (:style old-props)
        rets      (set/difference (set (keys old-style)) (set (keys styles)))
        styles'   (->> (reduce (fn [r k] (assoc r k nil)) styles rets)
                      #_ (reduce-kv (fn [r k v] (assoc r (name k) (name v))) {}))]
    #?(:cljs (sty/setStyle elem (clj->js styles')))))

(defn add-event-handler! [elem k f]
  #?(:cljs
     (let [sp         (shadow-props elem)
           event-name (name k)
           old-event  (get sp k)]
       ;; (js/console.log elem k event-name f)
       (when (and old-event (not= f old-event))
         (log/warn "Potential mistake: An event listener is being replaced. They should be constant. An anonymous function is probably created on each render. Hint: use `(partial merge {…})` or `(partial assoc {…} :key)`so the `{…}` map is constant."
                   {:prop k, :element (re-find #"^<[^>]*>" (.-outerHTML elem))})
         (events/unlisten elem event-name old-event))
       (events/listen elem event-name f))))

(defn remove-event-handler! [elem k]
  #?(:cljs
     (let [sp         (shadow-props elem)
           event-name (name k)
           old-event  (get sp k)]
       (when old-event
         (events/unlisten elem event-name old-event)))))

(def NON-STANDARD-ATTRIBUTES #{"list" ;; read-only, must use setAttribute
                               })

(defmulti set-attribute! (fn [_elem k _v] k))
(defmethod set-attribute! :default [elem k v]
  #?(:cljs
     (do
       ;; (prn elem k v)
       (if true #_(NON-STANDARD-ATTRIBUTES k)
         (xml/setAttributes elem (js-obj k v))
         #_(dom/setProperties elem (js-obj k v))))))

(defn ns-segments [named] (when named (str/split (name named) #"\.")))
(defn segments->ns [segments] (when (seq segments) (str/join "." segments)))

(defn select-ns [ns m]
  (reduce-kv (fn [r k v]
               (if (keyword? k)
                 (if-let [segments (ns-segments (namespace k))]
                   (if (= ns (first segments))
                     (assoc r (keyword (segments->ns (rest segments)) (name k)) v)
                     r)
                   r)
                 r))
             {} m))

(tests
  (select-ns "foo" {:foo/bar 1, :baz 2}) := {:bar 1}
  (select-ns "dom" {:dom.attribute/id 1}) := {:attribute/id 1}
  (->> {:dom.attribute.data/foo 1}
       (select-ns "dom")
       (select-ns "attribute")
       (select-ns "data")) := {:foo 1})

(defn rewrite-attr [attr]
  (if (keyword? attr)
    (if (#{"attribute.data" "attribute.aria"} (namespace attr))
      (keyword "attribute" (str (last (ns-segments (namespace attr))) "-" (name attr)))
      attr)
    attr))

(defn set-prop! [elem k v]
  #?(:cljs
     (try
       (let [sp (shadow-props elem)]
         (when (not= v (get sp k))
           (let [prop (name k)]
             (cond
               (= :attribute/style k)    (set-style! elem v)
               (= "event" (namespace k)) (add-event-handler! elem k v)
               :else                     (let [v (if (keyword? v) (name v) v)]
                                           (set-attribute! elem prop v))))))
       (catch js/Error e
         (js/console.error "Failed to set prop" {:elem elem, :key k, :value v :error e})))))

(defn patch-properties! [elem props]
  (let [old-props (shadow-props elem)
        rets      (set/difference (set (keys old-props)) (set (keys props)))]
    (when (seq rets)
      (run! (fn [k]
              (let [k (name k)]
                (if (= "event" (namespace k))
                  (remove-event-handler! elem k)
                  #?(:cljs (.removeAttribute elem (name k))))))
            rets))
    (run! (fn [[k v]]
            (set-prop! elem k (rewrite-attr v)))
          props)
    (set-shadow-props! elem props)))

;; f :: Flow {k a} -> (a -> Flow b) -> Flow {k b}
;; join-all :: Flow {k Flow a} -> Flow {k a}

;; switch :: Flow [Flow a] -> Flow [a] ; semi-lazy join
;; This impl is eager. Using this until we get cancellation on switch.
(defn switch' [>a]
  (m/ap (m/?< (m/?< >a))))

;; TODO shadow props
(defnode tag [name props children]
  (doto (create-tag-node name)
    (mount children)
    (patch-properties! (select-ns "dom" props))))

#_
(defn tag [name >props & >childs]
  (m/observe
   (fn [!]
     (let [elem            (create-tag-node name)
           children-stream (when (seq (filter identity >childs))
                             ;; if contains child -> replacechild
                             ;; else appendChild
                             (m/stream! (switch' (apply m/latest #(mount elem %&) >childs))))
           props-stream    (when >props (m/stream! (m/latest #(patch-properties! elem (select-ns "dom" %)) >props)))]
       (! elem)
       (fn []
         (when children-stream
           (children-stream))
         (when props-stream
           (props-stream))
         (run! (fn [k]
                 #?(:cljs (.removeAttribute elem k)))
               (set (keys (shadow-props elem))))
         (set-shadow-props! elem nil))))))

(defnode append-child! [parent child]
  (doto parent (mount [child])))

(defnode mount-component-at-node! [id component]
  (append-child! (by-id id) component))

(defn use-state
  ([] (use-state nil))
  ([initial-value]
   (let [!atom (atom initial-value)
         >atom (m/watch !atom)]
     [>atom #(reset! !atom %)])))

(defn noop [& _])

(defn component [{:keys [did-mount will-unmount render]
                  :or   {did-mount    noop
                         will-unmount noop}}]
  (fn [>props & children>]
    (m/latest (fn [elem] (do (did-mount elem) elem))
              (switch' (m/observe
                        (fn [!]
                          (let [elem (apply render >props children>)]
                            (! elem)
                            (fn []
                              (will-unmount elem)))))))))
(defn assoc-keys [m & kvs]
  (if (odd? (count kvs))
    (throw (ex-info "assoc-kvs takes an even number of key-values" {:m   m
                                                                    :kvs kvs}))
    (let [middle (/ (count kvs) 2)]
      (apply assoc m (interleave (take middle kvs) (drop middle kvs))))))

(tests
  (assoc-keys {} :foo :bar 1 2) := {:foo 1, :bar 2})

;;;;;;;;;;;
;; TESTS ;;
;;;;;;;;;;;

#?(:cljs
   (do
     (defn install-test-root! []
       (let [root (doto (.createElement js/document "div")
                    (.setAttribute "id" "hf-test-root"))]
         (.appendChild js/document.body root)
         root))

     (defn make-root! [cont]
       (prn "mount")
       (let [root (install-test-root!)]
         (cont)
         (prn "unmount")
         (.remove root)))

     (t/use-fixtures :once make-root!)

     (tests
       "Simple Rendering"
       (install-test-root!)
       (def content (tag :p nil (text (m/ap "foo"))))
       ((m/reactor (m/stream! (mount-component-at-node! "hf-test-root"  content)))
        js/console.log js/console.error)
       (def node (by-id "hf-test-root"))
       (def node-text (.-textContent node))
       node-text := "foo"

       )))
;; (set! hyperfiddle.rcf/*enabled* true)
