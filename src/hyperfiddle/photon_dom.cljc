(ns hyperfiddle.photon-dom
  (:refer-clojure :exclude [time for])
  (:require [contrib.cljs-target :refer [do-browser]]
            [hyperfiddle.photon :as p]
            [missionary.core :as m]
            #?(:cljs [goog.dom :as d])
            #?(:cljs [goog.events :as e])
            #?(:cljs [goog.object :as o])
            #?(:cljs [goog.style])
            [hyperfiddle.rcf :as rcf :refer [tests tap %]]
            [hyperfiddle.logger :as log]
            [clojure.string :as str])
  #?(:cljs (:require-macros [hyperfiddle.photon-dom :refer [with oget]]))
  #?(:cljs (:import (goog.ui KeyboardShortcutHandler)
                    (goog.ui.KeyboardShortcutHandler EventType))))

(defn command? "A command is a pair [:keyword any-value], describing an action."
  [x]
  (and (vector? x)
    (= (count x) 2)
    (keyword? (first x))))

(defn bubble? [x] (and (map? x) (::bubble (meta x))))
(defn bubble "Coerces to a map and tag it as bubble" [x]
  (if (bubble? x) x (vary-meta (if (map? x) x (into {} x)) assoc ::bubble true)))

(defn extract-commands [xs]
  (cond
    (command? xs)  [xs]
    (bubble? xs)     (extract-commands (vals xs))
    (sequential? xs) (transduce (comp (map extract-commands) (remove nil?) ) into [] xs)
    :else            []))

(defn mappend
  "Merge collection of commands."
  ([] [])
  ([tx] (extract-commands tx))
  ([tx & txs]
   (reduce into (mappend tx) (map mappend txs))))

(tests
  ;; boundaries
  (mappend)                       := []
  (mappend nil)                   := []
  (mappend [])                    := []
  (mappend {})                    := []
  (mappend ())                    := []
  (mappend #{})                   := []
  (mappend nil nil)               := []
  (mappend (bubble {}))           := []

  ;; correctness
  (mappend [[:a 1]])              := [[:a 1]]
  (mappend [[:a {:b 1}]])         := [[:a {:b 1}]]
  (mappend [[:a 1]] nil)          := [[:a 1]]
  (mappend [[:a 1]] [])           := [[:a 1]]
  (mappend [[:a 1]] [[:b 2]])     := [[:a 1] [:b 2]]
  (mappend (bubble {:a 1}))       := []
  (mappend (bubble {:a [:b 1]}))  := [[:b 1]]
  (mappend (bubble {:a [:b 1]})
           (bubble {:b [:c 2]}))  := [[:b 1] [:c 2]]

  ;; inverse
  (mappend nil [[:a 1]])          := [[:a 1]]
  (mappend [] [[:a 1]])           := [[:a 1]]
  (mappend [[:a 1]] [[:b 2]])     := [[:a 1] [:b 2]]

  ;; Nesting
  (mappend [[[:a 1]]])            := [[:a 1]]
  (mappend [[[:a 1]]] [[[:b 2]]]) := [[:a 1] [:b 2]]
  (mappend [[[:a 1]]]
    [[[(bubble {:a 1, :b [:c 3]})]]]
    [[[:b 2]]])                   := [[:a 1] [:c 3] [:b 2]]
  )

(defmacro bubbling [& body]
  (when (seq body)
    `(mappend ~@body)))

(def nil-subject (fn [!] (! nil) #()))
(p/def keepalive (new (m/observe nil-subject)))

(p/def node) ; used to be called parent

(defn by-id [id] #?(:cljs (js/document.getElementById id)))

(defn unsupported [& _]
  (throw (ex-info (str "Not available on this peer.") {})))

(def hook "See `with`"
  #?(:clj  unsupported
     :cljs (fn ([x] (.removeChild (.-parentNode x) x))    ; unmount
            ([x y] (.insertBefore (.-parentNode x) x y)) ; rotate siblings
             )))

(defmacro with
  "Attach `body` to a dom node, which will be moved in the DOM when body moves in the DAG.
  Given p/for semantics, `body` can only move sideways or be cancelled.
  If body is cancelled, the node will be unmounted.
  If body moves, the node will rotate with its siblings."
  [dom-node & body]
  `(binding [node ~dom-node]
     (new (p/hook hook node  ; attach body frame to dom-node.
            (p/fn [] keepalive ~@body) ; wrap body in a constant, making it a frame (static, non-variable), so it can be moved as a block.
            ))))

(defn dom-element [parent type]
  #?(:cljs (let [node (d/createElement type)]
             (.appendChild parent node) node)))

(defn text-node [parent]
  #?(:cljs (let [node (d/createTextNode "")]
             (.appendChild parent node) node)))

(defn set-text-content! [e & strs]
  #?(:cljs (set! (.-textContent e) (apply str strs)))
  nil)

(defmacro text [& strs]
  `(with (text-node node) (set-text-content! node ~@strs)))

(def text-literal? (some-fn string? number? char? boolean? ident?))

#?(:clj
   (defn handle-text [body]
     (some->> (filter some? body)
       (map (fn [form] (if (text-literal? form) ; TODO optimize further, detect dom/text and ^String tag meta
                         `(dom/text ~form)
                         `(let [res# ~form]
                            (if (text-literal? res#)
                              (do (dom/text res#)
                                  res#)
                              res#))))))))

(defmacro element [t & [props & body]]
  (let [[props body] (if (map? props) [props body] [nil (seq (cons props body))])
        body         (handle-text body)]
    `(with (dom-element node ~(name t))
       ~@(cond (map? props)  (cons `(props ~props) body)
               (some? props) (cons props body)
               :else         body))))

(defn set-style! [node k v]
  #?(:cljs (goog.style/setStyle node (name k) (clj->js v))))

(defn class-str [v]
  (cond
    (or (string? v) (keyword? v)) (name v)
    (seq v)                       (str/join " " (eduction (remove nil?) (map name) v))
    :else                         ""))

(tests
  (class-str nil)                    := ""
  (class-str [])                     := ""
  (class-str "x")                    := "x"
  (class-str ["x"])                  := "x"
  (class-str ["x" "y"])              := "x y"
  (class-str [nil])                  := ""
  (class-str ["x" nil "y"])          := "x y"
  (class-str [nil nil nil])          := ""
  (class-str (into-array [nil "x"])) := "x"
  (class-str [:x "y"])               := "x y")

(defn set-property! [node k v]
  #?(:cljs
     (let [k (name k)
           v (clj->js v)]
       (if (and (nil? v) (.hasAttribute node k))
         (.removeAttribute node k)
         (case k
           "style" (goog.style/setStyle node v)
           "class" (set! (.-className node) (class-str v))
           "for"   (set! (.-htmlFor node) v)
           "list"  (.setAttribute node k v) ; corner case, list (datalist) is setted by attribute and readonly as a prop.
           (if-let [k (o/get d/DIRECT_ATTRIBUTE_MAP_ k)]
             (.setAttribute node k v)
             (if (o/containsKey node k) ; is there an object property for this key?
               (o/set node k v)
               (.setAttribute node k v))))))))

(defn unmount-prop [node k v]
  (m/observe (fn [!] (! nil)
               #(set-property! node k v))))

(defmacro style [m]
  (if (map? m)
    `(do ~@(mapcat (fn [[k v]] [`(set-property! node "style" {~k ~v})
                                `(new (unmount-prop node "style" {~k nil}))]) m)
         nil) ; static keyset
    `(p/for-by first [sty# (vec ~m)]
       (set-property! node "style" {(key sty#) (val sty#)})
       (new (unmount-prop node {(key sty#) nil}))
       nil)))

;; TODO JS runtimes intern litteral strings, so call `name` on keywords at
;; macroexpension.
(defmacro props [m]
  (let [style? #{:style ::style}]       ; TODO disambiguate
    (if (map? m)
      `(do ~@(mapcat (fn [[k v]] (if (style? k) ; static keyset
                                   [`(style ~v)]
                                   [`(set-property! node ~k ~v)
                                    `(new (unmount-prop node ~k nil))]))
               m)
           nil)
      `(p/for-by key [prop# (vec ~m)]
         (if (~style? (key prop#))
           (style (val prop#))
           (do (set-property! node (key prop#) (val prop#))
             (new (unmount-prop node (key prop#) nil))
             nil))))))

(defn >events* [node event-type & [xform init rf :as args]]
  #?(:cljs (let [event-type (if (coll? event-type) (to-array event-type) event-type)
                 init?      (>= (count args) 2)]
             (cond->> (m/observe (fn [!] (e/listen node event-type !) #(e/unlisten node event-type !)))
               xform  (m/eduction xform)
               init?  (m/reductions (or rf {}) init)))))

(defmacro >events
  "Produce a discreet flow of dom events of `event-type` on the current dom node.
  `event-type` can be a string naming the event or a set of event names.
  If provided:
  - `xform` will be applied as an eduction,
  - `init`  will be the initial value of the flow,
  - `rf`    will be used as a reducing function over the flow, defaulting to `{}`.
  Also:
  - if `init` is not provided, the flow will not have an initial value,
  - `rf` is applied only if `init` is provided,
  - if `xform`, `init` and `rf` are provided, they form a transduction."
  [event-type & [xform init rf :as args]]
  (list* `>events* `node event-type args))

(defn >keychord-events* [node keychord & [xform init rf :as args]]
  (assert (or (string? keychord) (coll? keychord)))
  #?(:cljs (let [init? (>= (count args) 2)]
             (cond->> (m/observe (fn [!]
                                   (let [^js handler (new KeyboardShortcutHandler node)]
                                     (doseq [chord (if (string? keychord) #{keychord} keychord)]
                                       (try (.registerShortcut handler chord chord)
                                            (catch :default err
                                              (log/error (.-message err) ". Example of keychord identifiers: esc, enter, shift+a"))))
                                     ;; FIXME, desired behavior is `setModifierShortcutsAreGlobal` but it
                                     ;; doesn’t work with codemirror
                                     (.setAllShortcutsAreGlobal handler true)
                                     (e/listen handler "shortcut" !)
                                     #(e/unlisten handler "shortcut" !))))
               xform (m/eduction xform)
               init? (m/reductions (or rf {}) init)))))

(defmacro >keychord-events
  "Produce a discreet flow of key combo events. `keychord` is a string or a set of
  strings describing keychords, which looks like:
  - `\"a\"`
  - `\"t e s t\"`
  - `\"shift+f12\"`
  - `\"shift+f11 c\"`
  - `\"meta+y\"`
  @see https://github.com/google/closure-library/blob/master/closure/goog/demos/keyboardshortcuts.html
  "
  ;; TODO This implementation ignores focused form elements. For instance,
  ;; pressing SPACE on a button simulates a click and should not be tampered
  ;; with for accessibility reasons. In the meantime, please register keychords
  ;; at the right place in the dom tree to avoid event bubbling messing with
  ;; accessibility.
  [keychord & [xform init rf :as args]]
  (list* `>keychord-events* `node keychord args))

(defmacro events
  "Return a transduction of events as a continuous flow. See `clojure.core/transduce`.\n
   `event-type` can be a string like `\"click\"`, or a set of strings.

   ```clojure
   ;; count clicks
   (new (events \"click\" (map (constantly 1)) 0 +))

   ;; track focus state
   (new (events #{\"focus\" \"blur\"}
        (comp (map event-type) (map {\"focus\" true, \"blur\" false}))
        false))
   ```"
  ([event-type]                    `(new (m/relieve {} (>events* node ~event-type nil    nil   nil))))
  ([event-type xform]              `(new (m/relieve {} (>events* node ~event-type ~xform nil   nil))))
  ([event-type xform init]         `(new (m/relieve {} (>events* node ~event-type ~xform ~init nil))))
  ([event-type xform init rf]      `(new (m/relieve {} (>events* node ~event-type ~xform ~init ~rf))))
  ([node event-type xform init rf] `(new (m/relieve {} (>events* ~node  ~event-type ~xform ~init ~rf)))))

(defn event* [dom-node event-name callback]
  (m/observe (fn [!]
               (! nil)
               (.addEventListener dom-node event-name callback)
               #(.removeEventListener dom-node event-name callback))))

(defmacro event [event-name callback] `(new (event* node ~event-name ~callback)))

(defn flip [f] (fn [& args] (apply f (reverse args))))

(defn oget* [obj ks]
  #?(:clj  (get-in obj ks)
     :cljs (apply o/getValueByKeys obj (clj->js ks))))

(defn- path-ident? [x] (or (string? x) (simple-keyword? x)))

(defmacro oget
  "Like `aget`, but for js objects.
  Can be used in two ways:
  - direct call:         `(oget js-obj \"k1\" \"k2\" …)`
  - partial application: `(oget \"k1\" \"k2\" …)`"
  ;; TODO instead of calling `oget*`, expands to direct field access
  ;;      e.g.: obj["k1"]["k2"] -> obj.k1.k2
  [& args]
  (if (path-ident? (first args))
    (do (assert (every? path-ident? args))
        `(partial (flip oget*) [~@args]))
    (do (assert (every? path-ident? (rest args)))
        `(oget* ~(first args) [~@(rest args)]))))

;; Allows `get`, `get-in`, and keyword lookup on js objects, backed by
;; goog.object/getValueByKeys. Does not alter the global js Object prototype
;; (this is clean). Prefer `.-` or `..` access for performances.
;; E.g.: (:foo #js {:foo 1})
#?(:cljs
   (extend-protocol ILookup
     object
     (-lookup
       ([m k] (oget* m [k]))
       ([m k not-found] (or (oget* m [k]) not-found)))))

(defn oset!* [obj path val]
  #?(:clj  (assoc-in obj path val)
     :cljs (do (if (= 1 (count path))
                 (o/set obj (clj->js (first path)) val)
                 (oset!* (oget* obj (butlast path)) [(last path)] val))
               obj)))

; dom/oset! has external usage
(defmacro ^:deprecated oset! [& args]
  (if (path-ident? (first args)) ; partial application code path
    (do (assert (every? path-ident? (butlast args)))
        `(partial (flip oset!) ~(last args) [~@(butlast args)]))
    (do (assert (every? path-ident? (butlast (rest args))))
        `(oset!* ~(first args) ; dom/node
                 [~@(butlast (rest args))] ; paths
                 ~(last args))))) ; value

(comment (dom/oset! dom/node :value ""))

(defn stop-event! [event]
  (.preventDefault event)
  (.stopPropagation event)
  event)

(defn focus-state [e]
  (>events* e #{"focus" "blur"}
      (comp (map (oget :type))
            (map {"focus" true, "blur" false}))
    false))

#?(:cljs
   (deftype Clock [^:mutable ^number raf
                   ^:mutable callback
                   terminator]
     IFn                                                    ; cancel
     (-invoke [_]
       (if (zero? raf)
         (set! callback nil)
         (do (.cancelAnimationFrame js/window raf)
             (terminator))))
     IDeref                                                 ; sample
     (-deref [_]
       ; lazy clock, only resets once sampled
       (if (nil? callback)
         (terminator)
         (set! raf (.requestAnimationFrame js/window callback))) ; RAF not called until first sampling
       ::tick)))

(def <clock "lazy & efficient logical clock that schedules no work unless sampled."
  #?(:cljs (fn [n t]
             (let [cancel (->Clock 0 nil t)]
               (set! (.-callback cancel)
                     (fn [_] (set! (.-raf cancel) 0) (n)))
               (n) cancel))
     #_#_:clj  (m/ap (loop [] (m/amb ::tick (do (m/? (m/sleep 1)) (recur)))))))

(comment
  ; This ticker will skew and therefore is not useful other than as an example.
  ; It's also dangerous, because if you write a program that does (prn ticker) it will run forever
  (p/def ticker (new (->> <clock
                          (m/eduction (map (constantly 1)))
                          (m/reductions + 0))))
  (tests (rcf/with (p/run (tap ticker)) [% % %] := [1 2 3])))

(defn -get-system-time-ms [_] #?(:clj (System/currentTimeMillis) :cljs (js/Date.now)))

(p/def system-time-ms "ms since 1970 Jan 1"
  (new (m/sample -get-system-time-ms <clock)))

(p/def system-time-secs "seconds since 1970 Jan 1"
  (Math/round (/ system-time-ms 1000.0)))

#?(:cljs
   (do-browser
     (tests
       "millisecond time as a stable test"
       (let [dispose (p/run (tap system-time-ms))]
         [% % %] := [_ _ _]
         (map int? *1) := [true true true]
         (dispose)))))

;(defn truncate-secs [max-hz secs]
;  (let [period (/ 1 max-hz)]
;    (* (quot secs period) period)))
;
;(tests
;  (truncate-secs 0.01 1670619905.404) := 1.6706199E9
;  (truncate-secs 0.1 1670619905.404)  := 1.67061990E9
;  (truncate-secs 1 1670619905.404)    := 1.670619905E9
;  (truncate-secs 10 1670619905.404)   := 1.6706199054E9
;  (truncate-secs 100 1670619905.404)  := 1.6706199054E9)
;
;(p/defn ^:deprecated Clock
;  "Seconds since 1970 Jan 1, truncated to frequency.
;  Because the clock is backpressured by requestAnimationFrame, this is format only and you should
;  just format the float as a string directly instead of use this. The computer will animate it
;  at low priority animation speed and even pause animations when the tab is not focused."
;  [Hz]
;  (let [secs (/ system-time-ms 1000)]
;    (case Hz 0 secs (truncate-secs Hz secs))))

(defn pack-string-litterals [xs]
  (loop [r        []
         [x & xs] xs]
    (cond
      (empty? xs) (conj r x)
      (and (string? x) (string? (first xs))) (recur (conj r (cons `str (cons x (take-while string? xs))))
                                               (drop-while string? xs))
      :else (recur (conj r x) xs))))

(defn- new-invoke? [x]
  (and (symbol? x) (= \. (last (name x)))))

(defn hiccup* [form]
  (cond
    (vector? form)  (cond
                      (new-invoke? (first form)) (cons (first form) (map hiccup* (rest form)))
                      (keyword? (first form))    (let [[tag & content]   form
                                                       [props & content] (pack-string-litterals content)
                                                       body              (cond (map? props) (cons props (map hiccup* content))
                                                                               (nil? props) (map hiccup* content)
                                                                               :else        (map hiccup* (cons props content)))]
                                                   (cond
                                                     (= :text tag) `(text ~@body)
                                                     :else         (list* `element (name tag) body)))
                      :else                      (mapv hiccup* form))
    (keyword? form) `(text ~(name form))
    (seq? form)     form
    :else           `(text ~form)))

(defmacro hiccup [form] (hiccup* form))

(defmacro a [& body] `(element :a ~@body))
(defmacro abbr [& body] `(element :abbr ~@body))
(defmacro address [& body] `(element :address ~@body))
(defmacro area [& body] `(element :area ~@body))
(defmacro article [& body] `(element :article ~@body))
(defmacro aside [& body] `(element :aside ~@body))
(defmacro audio [& body] `(element :audio ~@body))
(defmacro b [& body] `(element :b ~@body))
(defmacro bdi [& body] `(element :bdi ~@body))
(defmacro bdo [& body] `(element :bdo ~@body))
(defmacro blockquote [& body] `(element :blockquote ~@body))
(defmacro br [& body] `(element :br ~@body))
(defmacro button [& body] `(element :button ~@body))
(defmacro canvas [& body] `(element :canvas ~@body))
(defmacro cite [& body] `(element :cite ~@body))
(defmacro code [& body] `(element :code ~@body))
(defmacro data [& body] `(element :data ~@body))
(defmacro datalist [& body] `(element :datalist ~@body))
(defmacro del [& body] `(element :del ~@body))
(defmacro details [& body] `(element :details ~@body))
(defmacro dfn [& body] `(element :dfn ~@body))
(defmacro dialog [& body] `(element :dialog ~@body))
(defmacro div [& body] `(element :div ~@body))
(defmacro dl "The <dl> HTML element represents a description list. The element encloses a list of groups of terms (specified using the <dt> element) and descriptions (provided by <dd> elements). Common uses for this element are to implement a glossary or to display metadata (a list of key-value pairs)." [& body] `(element :dl ~@body))
(defmacro dt "The <dt> HTML element specifies a term in a description or definition list, and as such must be used inside a <dl> element. It is usually followed by a <dd> element; however, multiple <dt> elements in a row indicate several terms that are all defined by the immediate next <dd> element." [& body] `(element :dt ~@body))
(defmacro dd "The <dd> HTML element provides the description, definition, or value for the preceding term (<dt>) in a description list (<dl>)." [& body] `(element :dd ~@body))
(defmacro em [& body] `(element :em ~@body))
(defmacro embed [& body] `(element :embed ~@body))
(defmacro fieldset [& body] `(element :fieldset ~@body))
(defmacro figure [& body] `(element :figure ~@body))
(defmacro footer [& body] `(element :footer ~@body))
(defmacro form [& body] `(element :form ~@body))
(defmacro h1 [& body] `(element :h1 ~@body))
(defmacro h2 [& body] `(element :h2 ~@body))
(defmacro h3 [& body] `(element :h3 ~@body))
(defmacro h4 [& body] `(element :h4 ~@body))
(defmacro h5 [& body] `(element :h5 ~@body))
(defmacro h6 [& body] `(element :h6 ~@body))
(defmacro header [& body] `(element :header ~@body))
(defmacro hgroup [& body] `(element :hgroup ~@body))
(defmacro hr [& body] `(element :hr ~@body))
(defmacro i [& body] `(element :i ~@body))
(defmacro iframe [& body] `(element :iframe ~@body))
(defmacro img [& body] `(element :img ~@body))
(defmacro input [& body] `(element :input ~@body))
(defmacro ins [& body] `(element :ins ~@body))
(defmacro kbd [& body] `(element :kbd ~@body))
(defmacro label [& body] `(element :label ~@body))
(defmacro legend [& body] `(element :legend ~@body))
(defmacro li [& body] `(element :li ~@body))
(defmacro link [& body] `(element :link ~@body))
(defmacro main [& body] `(element :main ~@body))
#_(defmacro map [& body] `(element :map ~@body))
(defmacro mark [& body] `(element :mark ~@body))
(defmacro math [& body] `(element :math ~@body))
(defmacro menu [& body] `(element :menu ~@body))
(defmacro itemprop [& body] `(element :itemprop ~@body))
(defmacro meter [& body] `(element :meter ~@body))
(defmacro nav [& body] `(element :nav ~@body))
(defmacro noscript [& body] `(element :noscript ~@body))
(defmacro object [& body] `(element :object ~@body))
(defmacro ol [& body] `(element :ol ~@body))
(defmacro option [& body] `(element :option ~@body))
(defmacro output [& body] `(element :output ~@body))
(defmacro p [& body] `(element :p ~@body))
(defmacro picture [& body] `(element :picture ~@body))
(defmacro pre [& body] `(element :pre ~@body))
(defmacro progress [& body] `(element :progress ~@body))
(defmacro q [& body] `(element :q ~@body))
(defmacro ruby [& body] `(element :ruby ~@body))
(defmacro s [& body] `(element :s ~@body))
(defmacro samp [& body] `(element :samp ~@body))
(defmacro script [& body] `(element :script ~@body))
(defmacro section [& body] `(element :section ~@body))
(defmacro select [& body] `(element :select ~@body))
(defmacro slot [& body] `(element :slot ~@body))
(defmacro small [& body] `(element :small ~@body))
(defmacro span [& body] `(element :span ~@body))
(defmacro strong [& body] `(element :strong ~@body))
(defmacro sub [& body] `(element :sub ~@body))
(defmacro sup [& body] `(element :sup ~@body))
(defmacro table [& body] `(element :table ~@body))
(defmacro tbody [& body] `(element :tbody ~@body))
(defmacro td [& body] `(element :td ~@body))
(defmacro th [& body] `(element :th ~@body))
(defmacro thead [& body] `(element :thead ~@body))
(defmacro tr [& body] `(element :tr ~@body))
(defmacro template [& body] `(element :template ~@body))
(defmacro textarea [& body] `(element :textarea ~@body))
(defmacro time [& body] `(element :time ~@body))
(defmacro u [& body] `(element :u ~@body))
(defmacro ul [& body] `(element :ul ~@body))
(defmacro var [& body] `(element :var ~@body))
(defmacro video [& body] `(element :video ~@body))
(defmacro wbr [& body] `(element :wbr ~@body))


(defmacro context [type]
  `(.getContext node ~(name type)))

(defn happen [s e]
  (case (:status s)
    :idle {:status :impulse :event e} ; no impulse if not idle !
    s))

; data EventState = Idle | Impulse event | Pending event
(p/defn Event [type busy]
  (:event
    (let [!state (atom {:status :idle})
          state (p/watch !state)]

      ; rising edge happens once, even if busy state (prevent infinite loop)
      (event type (partial swap! !state happen)) ; discrete impulse

      (reset! !state
              (case (:status state)
                :idle state
                :impulse (assoc state :status :pending) ; impulse is seen for 1 frame and then cleared
                :pending (if busy state {:status :idle}))))))

(comment
  (Event. "click" false) ; impulse processed synchronously

  (p/with-cycle [busy false]
    (try
      (when-some [e (some? (Event. "click" busy))]
        (p/server (println 'clicked)))
      false
      (catch Pending e true))))

;(p/defn EventBroken [type busy]
;  (:event
;    (with-cycle [s {:status :idle}]
;                (let [e (events type)] ; in CT this event is never cleared, causing a loop
;                  (case (:status s)
;                    :idle (if (some? e) {:status :impulse :event e} s)
;                    :impulse (assoc s :status :pending)
;                    :pending (if busy s {:status :idle}))))))

(defmacro measure ; could this be replaced by watching computed style?
  "Given a css size like \"1em\", \"1rem\" or other units, return the
  corresponding size in pixels. Will install a invisible div in the DOM, at
  point."
  [size]
  `(first (div {::style {:height     ~size
                         :width      0
                         :outline    :none
                         :border     :none
                         :padding    :none
                         :margin     :none
                         :box-sizing :content-box
                         :visibility :hidden
                         :position   :absolute
                         }}
            [(.-offsetHeight dom/node)])))
