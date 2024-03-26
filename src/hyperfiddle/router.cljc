(ns hyperfiddle.router
  "A reactive tree router"
  (:refer-clojure :exclude [set])
  (:require
   [contrib.data]
   [contrib.sexpr-router :as sexpr]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.rcf :refer [tests]]
   [hyperfiddle.history2 :as h]
   [missionary.core :as m])
  #?(:cljs (:import [hyperfiddle.history2.HTML5History]))
  #?(:cljs (:require-macros hyperfiddle.router)))

(comment
  (hyperfiddle.rcf/enable! false))

;;; Minimal Lenses implementation

;; Slightly different than Haskell's
;; Haskell lenses's `view` is implemented with the Const functor.
;; We don't have fmap, so we use a two-arity function to separate read from write

;; Goal is to focus on one route, subroute, sub-subroute, etc. in the route tree.
;; So we can define a path and focus at that point.
;; We also want `.`, `..` (up) and `/` (root/absolute):
;; - `.` is the identity lens
;; - lenses cannot model `..` and `/`:
;;   - even profunctor-based lenses: they would produce the right structure but would loose context.
;;   - a "Path" monad would work
;;   - pragmatism: have a path as a stack in dynamic scope
(defn lens [getter-f setter-f]
  (fn [next-step-f]
    (fn ([value] (next-step-f (getter-f value)))
      ([value f] (setter-f value (fn [value] (next-step-f value f)))))))

(defn view [lens structure] ((lens identity) structure))
(defn identity-setter [structure f] (f structure))
(defn over [lens f structure] ((lens identity-setter) structure f))
(defn set [lens value structure] (over lens (constantly value) structure))
(def id-lens (lens identity identity-setter))

(defn key-lens [key]
  (lens
    (fn [structure] (get structure key))
    (fn [structure f] (update structure key f))))

(tests
  (view id-lens :foo)     := :foo
  (set id-lens :bar :foo) := :bar

  (view (key-lens :foo) {:foo 1})     := 1
  (set (key-lens :foo) 42 {:foo 1})   := {:foo 42}
  (over (key-lens :foo) inc {:foo 1}) := {:foo 2}

  (view (comp (key-lens :foo) (key-lens :bar)) {:foo {:bar 1}})     := 1
  (set (comp (key-lens :foo) (key-lens :bar)) 42 {:foo {:bar 1}})   := {:foo {:bar 42}}
  (over (comp (key-lens :foo) (key-lens :bar)) inc {:foo {:bar 1}}) := {:foo {:bar 2}}
)

(defn pad [n coll] (vec (contrib.data/pad (max n (count coll)) nil coll)))

(defn adaptive-key
  "Return a lens focusing on the value at `key` in an associative data structure.
  When updating the focused value by a function `f`:
  - if value is a map, then the result is equivalent to `{key (f value)}` (trivial case),
  - if value is vector, then `key` must be a natural integer. The value at
  position key will be updated by f. If `key` is out of the array upper bound,
  the vector will be resized and padded with `nil` .
  - if value is neither a map nor a vector, then value will become {value (f
  nil)}.
  - if value is `nil`:
    - if `key` is a natural integer, then value will default to a vector,
    - otherwise value will default to a map.
  "
  [key]
  (lens
    (fn [value] (get value key))
    (fn [value f]
      (cond
        (map? value)    (update value key f)
        (vector? value) (update (pad key value) key f)
        (nil? value)    (if (nat-int? key)
                          (update (pad key []) key f)
                          {key (f nil)})
        :else           (if (nat-int? key)
                          (update (pad key [value]) key f)
                          {value (f nil)})))))

(defn path-lens
  "Given a sequence of keys in an associative data structure (e.g. [:foo :bar
  0 :baz]), return a lens focusing on the value at the end of the path. An empty
  path (e.g. []) is an identity."
  [path]
  ;; TODO resolve path here too?
  (cond
    (nil? path)    id-lens
    (vector? path) (reduce comp (map adaptive-key path))
    :else          (adaptive-key path)))

(tests
  (set (path-lens nil) :bar :foo)                := :bar
  (set (path-lens []) :bar :foo)                 := :bar
  (set (path-lens :foo) :baz {:foo :bar})        := {:foo :baz}
  (set (path-lens [:foo :bar]) :baz {:foo nil})  := {:foo {:bar :baz}}
  (set (path-lens [:foo 0]) :bar {:foo []})      := {:foo [:bar]}
  (set (path-lens [:foo 1]) :bar {:foo []})      := {:foo [nil :bar]}
  (set (path-lens [:foo 0]) :bar {:foo nil})     := {:foo [:bar]}
  (set (path-lens [:foo 1]) :bar {:foo nil})     := {:foo [nil :bar]}
  (set (path-lens [:foo :bar]) :baz {:foo :bar}) := {:foo {:bar :baz}}
  (set (path-lens [:foo 0]) :bar {:foo :bar})    := {:foo [:bar]}
  (set (path-lens [:foo 1]) :baz {:foo :bar})    := {:foo [:bar :baz]}
  )

;;; Relative and absolute paths

(defn safe-pop [coll] (if (empty? coll) coll (pop coll)))

(defn resolve-path
  "Given a path eventually containing relative path components (e.g. `'.`, `'..`, or `'/`), resolve the final path.
  - `.` is a noop,
  - `..` navigates one level up,
  - `/` navigates to the root.
  e.g.
  [:foo '. :bar]       => [:foo :bar]
  [:foo '. '. :bar]    => [:foo :bar]
  [:foo :bar '..]      => [:foo]
  [:foo :bar '.. :baz] => [:foo :baz]
  ['.. :foo]           => [:foo]
  ['/]                 => []
  [:foo '/]            => []
  [:foo :bar '/ :baz]  => [:baz]
  "
  ([path]
   (if (seq path)
     (resolve-path [] path)
     []))
  ([stack [x & xs]]
   (if (seq xs)
     (case x
       .  (resolve-path stack xs)
       .. (resolve-path (safe-pop stack) xs)
       /  (resolve-path [] xs)
       (resolve-path (conj stack x) xs))
     (case x
       .  stack
       .. (safe-pop stack)
       /  []
       (conj stack x)))))

(tests
  (resolve-path [])                        := []
  (resolve-path [:foo])                    := [:foo]
  (resolve-path [:foo :bar :baz])          := [:foo :bar :baz]
  (resolve-path ['. :foo '. :bar '. :baz]) := [:foo :bar :baz]
  (resolve-path [:foo :bar '.. :baz])      := [:foo :baz]
  (resolve-path ['.. :foo])                := [:foo]
  (resolve-path ['.. '.. :foo])            := [:foo]
  (resolve-path [:foo '/ :bar])            := [:bar]
  (resolve-path ['/])                      := []
  (resolve-path [:foo '/])                 := []
  )

;;; human-friendly representation

(defn simplify [route]
  (cond
    (nil? route)                      ()
    (and (map? route) (empty? route)) ()

    (and (map? route) (= 1 (count route)))
    (let [k (key (first route))]
      (concat (if k [k] []) (simplify (val (first route)))))

    :else (list route)))

(tests
  "base case"
  (simplify nil) := ()
  (simplify {nil nil}) := ()
  (simplify {})  := ()
  (simplify [])  := '([])
  (simplify #{}) := '(#{})

  "single branch tree"
  (simplify '{page nil})                  := '(page)
  (simplify '{page subpage})              := '(page subpage)
  (simplify '{page {subpage subsubpage}}) := '(page subpage subsubpage)

  "branching at the root"
  (simplify '{foo 1, bar 2}) := '({foo 1, bar 2})

  "branching at level x"
  (simplify '{pages {1 foo, 2 bar}}) := '(pages {1 foo, 2 bar})
  (simplify '{revisions {diff {left-sha  1234,
                               right-sha 4321}}})
  := '(revisions diff {left-sha 1234, right-sha 4321})

  "Sexprs as keys"
  (simplify '{(f x) y})      := '((f x) y)
  (simplify '{hfql {(f x) y}}) := '(hfql (f x) y)
  (simplify '{hfql {(f x) y
                    (g x) z}}) := '(hfql {(f x) y, (g x) z})

  )

(defn normalize [x]
  (cond
    (nil? x) nil
    (map? x) (not-empty (update-vals x (fn [x]
                                         (if (seq? x)
                                           {x nil}
                                           (normalize x)))))
    (seq? x) (cond
               (empty? x)       nil
               (map? (first x)) (normalize (first x))
               :else            {(first x) (normalize (next x))})
    :else    {x nil}))


(tests
  "base case"
  (normalize ()) := nil
  (normalize nil) := nil
  (normalize {})  := nil
  (normalize [])  := {[] nil}
  (normalize #{}) := {#{} nil}

  "single branch tree"
  (normalize '(page))                    := '{page nil}
  (normalize '(page subpage))            := '{page {subpage nil}}
  (normalize '(page subpage subsubpage)) := '{page {subpage {subsubpage nil}}}

  "branching at the root"
  (normalize '({page 1})) := '{page {1 nil}}

  "sexpr routes with nesting"
  (normalize '({(foo bar) baz})) := '{(foo bar) {baz nil}}
  (normalize '((foo bar) baz))   := '{(foo bar) {baz nil}}

  "branching at level 1"
  (normalize '((foo) {1 bar, 2 baz})) := '{(foo) {1 {bar nil}, 2 {baz nil}}}
  (normalize '(revisions diff {left-sha  1234,
                               right-sha 4321}))
  := '{revisions {diff {left-sha {1234 nil}, right-sha {4321 nil}}}}

  ;; TODO revise if we can use vectors in the route shape
  ;; (normalize '[revisions diff [1234 4321]]) := '{revisions {diff [{1234 nil} {4321 nil}]}}
  (normalize '(revisions diff [1234 4321])) := '{revisions {diff {[1234 4321] nil}}}

  (normalize '(pages {1 foo} {2 bar})) := '{pages {1 {foo nil}}} ; only keep first tree line

  "Sexprs as keys"
  (normalize '({(f x) y}))      := '{(f x) {y nil}}
  (normalize '(hfql {(f x) y})) := '{hfql {(f x) {y nil}}}
  (normalize '((f x) {(g y) (h z)})) := '{(f x) {(g y) {(h z) nil}}}
  )

(tests
  (normalize (simplify nil)) := nil
  (normalize (simplify {}))  := nil
  (normalize (simplify []))  := {[] nil}
  (normalize (simplify #{})) := {#{} nil}

  (normalize (simplify '{page nil}))                  := '{page nil}
  (normalize (simplify '{page subpage}))              := '{page {subpage nil}}
  (normalize (simplify '{page {subpage subsubpage}})) := '{page {subpage {subsubpage nil}}}
  (normalize (simplify '{foo 1, bar 2}))         := '{foo {1 nil}, bar {2 nil}}
  (normalize (simplify '{pages {1 foo, 2 bar}})) := '{pages {1 {foo nil}, 2 {bar nil}}}
  (normalize (simplify '{revisions {diff {left-sha  1234,
                                          right-sha 4321}}}))
  := '{revisions {diff {left-sha  {1234 nil}, right-sha {4321 nil}}}}

  (normalize (simplify '{hfql {(f x) y (g x) z}}))
  := '{hfql {(f x) {y nil}, (g x) {z nil}}}
  )


;;; URL encoding

(defn encode* [route] (sexpr/encode (not-empty (simplify route))))
(defn decode* [path] (normalize (sexpr/decode path)))

(tests
  (decode* (encode* nil)) := nil
  (decode* (encode* {}))  := nil
  (decode* (encode* []))  := {[] nil}
  (decode* (encode* #{})) := {#{} nil}

  (decode* (encode* '{page nil}))              := '{page nil}
  (decode* (encode* '{page subpage}))          := '{page {subpage nil}}
  (decode* (encode* '{page {subpage subsubpage}})) := '{page {subpage {subsubpage nil}}}
  (decode* (encode* '{foo 1, bar 2}))          := '{foo {1 nil}, bar {2 nil}}
  (decode* (encode* '{pages {1 foo, 2 bar}}))  := '{pages {1 {foo nil}, 2 {bar nil}}}
  (decode* (encode* '{revisions {diff {left-sha  1234,
                                       right-sha 4321}}}))
  := '{revisions {diff {left-sha  {1234 nil},
                        right-sha {4321 nil}}}}
  (decode* (encode* '{hfql {(f x) y (g x) z}})) := '{hfql {(f x) {y nil} (g x) {z nil}}}
  )

;;; Electric

(e/def paths "A stack of paths" [])
(e/def path "The current path" [])
(e/def root-route "Top level route" nil)
(e/def route "Current rout in the scope of a router" nil)

(e/def encode encode*)
(e/def decode decode*)

(defn current-path [paths] (into [] cat paths))

(defn as-vec [x] (if (vector? x) x [x]))

(e/defn Focus [path Body-fn]
  (let [paths (conj paths (as-vec path))
        path (resolve-path (current-path paths))]
    (binding [hyperfiddle.router/paths paths
              hyperfiddle.router/path  path
              route (view (path-lens path) root-route)]
      (Body-fn.))))

(defmacro focus [path & body] `(new Focus ~path (e/fn* [] ~@body))) ;; TODO find a better name

(defn split-link-path [path]
  (cond (not (vector? path)) [['.] path]
        (= 1 (count path))   [['.] (first path)]
        :else                [(vec (butlast path)) (last path)]))

(e/defn Route-for
  ([path] (e/apply Route-for (split-link-path path)))
  ([path value]
   (over (path-lens (resolve-path (into hyperfiddle.router/path path))) (constantly value) root-route)))


#_ ;; l/local expects l/def which we should only use in tests
(tests ;; static routing
  (with ((l/local
           (binding [root-route nil]
             (focus 'index
               (tap path)
               (focus 'foo
                 (tap path)
                 (tap (Route-for. "foo")))
               (focus 'bar
                 (tap path)
                 (tap (Route-for. "bar"))))))))
  % := '[index]
  % := '[index foo]
  % := '{index {foo "foo"}}
  % := '[index bar]
  % := '{index {bar "bar"}}
  )

;; (tests ;; dynamic routing
;;   (binding [root-route '{users {12 profile, 42 profile}}]
;;     (focus 'users
;;       (fn []
;;         (tap route)
;;         (doall
;;           (for [[user-id _] route]
;;             (focus [user-id 'profile]
;;               (fn []
;;                 (tap path)
;;                 (tap (route-for "settings")))))))))
;;   % := '{12 profile, 42 profile}
;;   % := '[users 12 profile]
;;   % := '{users {12 {profile "settings"}, 42 profile}}
;;   % := '[users 42 profile]
;;   % := '{users {12 profile, 42 {profile "settings"}}}
;;   )

(e/defn Route-at [path]
  (view (path-lens (resolve-path (into hyperfiddle.router/path path))) root-route))

;; (tests
;;   (binding [root-route '{index {foo 1, bar 2}}]
;;     (focus 'index
;;       (fn []
;;         (tap path)
;;         (tap route)
;;         (focus 'foo
;;           (fn []
;;             (tap path)
;;             (tap route)
;;             (tap (route-at '[.. bar]))
;;             (tap (route-for '[.. bar] "altered-by-sibling")))))))
;;   % := '[index]
;;   % := '{foo 1, bar 2}
;;   % := '[index foo]
;;   % := 1
;;   % := 2
;;   % := '{index {foo 1, bar "altered-by-sibling"}}
;;   )


(e/defn Current-route? [target-route]
  (= (resolve-path (current-path paths))
    (resolve-path (current-path (conj paths (as-vec target-route))))))

;; (tests
;;   (focus '[page subpage]
;;     (fn []
;;       (tap (current-route? '.))
;;       (tap (current-route? 'subsubpage))
;;       (focus 'subsubpage
;;         (fn []
;;           (tap (current-route? '[.. .. .. page subpage subsubpage]))))))
;;   % := true
;;   % := false
;;   % := true)

;;; Link

#?(:cljs
   (defn on-link-click [next-route ^js node ^js e]
     ;; enrich click event with:
     ;; - the route
     ;; - is the link internal or external (soft vs hard nav)
     (let [target (.getAttribute node "target")]
       (set! (.-hyperfiddle_router_route e) next-route)
       (set! (.-hyperfiddle_router_external_nav e)
         (or (some? (.getAttribute node "download"))
           (and (some? target) (not= "_self" target))))
       nil)))

#?(:cljs
   (defn link-click-handler [node next-route-f]
     (m/relieve {}
       (m/reductions {} nil
         (e/listen> node "click"
           (fn [e] ; todo e/for-event-pending-switch? Or missionary itself
             (on-link-click (next-route-f) node e)))))))

(e/def current-route? false)

(defn normalize-route-value [x]
  (if (seq? x)
    (normalize {x nil})
    (normalize x)))

(e/defn Link
  ([path Body]
   (e/client
     (let [[path' value] (split-link-path path)
           value (normalize-route-value value)]
       (dom/a
         (let [!path (atom nil)]
           (reset! !path (into hyperfiddle.router/path path)) ; prevents event handler to remount on path change
           (new (link-click-handler dom/node #(deref !path))))
         (dom/props {::dom/href (encode (Route-for. path' value))})
         (binding [current-route? (Current-route?. path')]
           (new Body)))))))

(defmacro link [path & body]
  `(new Link ~path (e/fn* [] ~@body)))

;;; History integration

(e/defn OnBeforeNavigate! "Run for effect on history navigation" [])
(e/def confirm-navigation?
  "A predicate called on user navigation intent. If false, the current navigation intent is prevented.
   Called during DOM event bubbling phase, it must be synchronous and therefore must be bound to a clojure function."
  (fn [_dom-event] true))

#?(:cljs
   (defn- internal-nav-intent? [^js e]
     (and (some? (.-hyperfiddle_router_route e))
       (not (.-hyperfiddle_router_external_nav e)))))

(e/defn OnNavigate
  "Will call `Callback` on internal `router/Link` click. `Callback` takes 2 arguments:
   - the route to navigate to
   - the dom click js event."
  ([Callback] (e/client (OnNavigate. (.-document js/window) Callback)))
  ([node Callback]
   (e/client
     ;; navigation by link click (also supports keyboard nav)
     ;; only intercepts internal links. See `hyperfiddle.router/Link`.
     ;; 1. We want to cancel native navigation ASAP if needed. We want a synchronous event handler.
     ;;    dom/on! – guarantees the event will be canceled before it bubbles up to the parent
     ;;    dom/on  – callback is async and might cancel the event too late, especially if the reactor is busy
     (dom/on! node "click"  (fn* [^js e] (when (internal-nav-intent? e) (.preventDefault e))))
     ;; 2. Then we can handle the event asynchronously to perform the navigation (or not)
     (dom/on node "click"
       (e/fn* [^js e]
         (when (and (internal-nav-intent? e) (confirm-navigation? e))
           (case (OnBeforeNavigate!.)  ; sequence effects
             (Callback. (.-hyperfiddle_router_route e) e))))))))

(e/defn Navigate! [path]
  (h/navigate! h/history (encode (Route-for. path))))

(e/defn ReplaceState! [path] ;; TODO find a better name
  (h/replace-state! h/history (encode (Route-for. path))))

(e/defn HTML5-Navigation-Intents [^HTML5History history]
  (e/client
    (let [!idle (atom false)]
      (try
        (dom/on js/window "beforeunload" ; refresh or close tabe
          (e/fn* [^js e]
            (when-not (confirm-navigation? e)
              (.preventDefault e))))

        ;; (OnNavigate. (e/fn* [route _event]
        ;;                  (binding [h/history history] (Navigate!. route))))

        (dom/on js/window "popstate"    ; previous and next button
          (e/fn* [^js e]
            ;; "popstate" event can't be cancelled. We are forced to detect
            ;; navigation direction (back/forward) and to invert it. History
            ;; must be idle during this back and forth operation to prevent a
            ;; page flicker.
            (when-let [curr-position (some-> e .-state .-position)]
              (let [stack         @(.-!stack history)
                    prev-position @(.-!position history)]
                (reset! (.-!position history) curr-position)
                (let [delta (h/nav-delta stack prev-position curr-position)]
                  (cond
                    @!idle (reset! !idle false)
                    (confirm-navigation? e) (OnBeforeNavigate!.)
                    :else (do (reset! !idle true)
                              (.. js/window -history (go (- delta))))))))))

        (catch hyperfiddle.electric.Pending _)) ; temporary hack, fixes page reload on click, needs sync on dom/on, hf/branch, and Pending interaction
      (e/watch !idle))))

;; (defonce html5-history-singleton (atom nil))

(e/defn HTML5-History []
  (e/client
    #_(if-let [history @html5-history-singleton]
      history
      (reset! html5-history-singleton))
    (let [history (h/html5-history)]
      (when-not (HTML5-Navigation-Intents. history) ; idles history while user confirms navigation
        (new (m/observe (fn [!]
                          (! nil)
                          (let [f (fn [_e]
                                    (reset! (h/-html5-history-get-state history) (h/html5-path)))]
                            (f nil)
                            (.addEventListener js/window "popstate" f)
                            #(.removeEventListener js/window "popstate" f))))))
      history)))

(e/defn Router [history BodyFn]
  (binding [h/history  history
            root-route (decode (e/watch history))
            paths      []]
    (OnNavigate. dom/node (e/fn* [route e] (Navigate!. route)))
    (focus '/
      (BodyFn.))))

(defmacro router [history & body]
  `(new Router ~history (e/fn* [] ~@body)))

(defn update-key
  "Update a key in a map. Replaces key `k` in map `m` by the result of `(f k)`.
  The corresponding value is preserved."
  ;; Useful for HFQL and `demo-colors`
  [m k f]
  (let [k' (f k)
        m' (assoc m k' (get m k))]
    (if (= k' k)
      m'
      (dissoc m' k))))

(defn set-key "Replaces `old-key` in map `m` by `new-key` without altering the corresponding value.
  Similar to `clojure.set/rename-keys`, but faster and for a single key."
  ;; Useful for HFQL and `demo-colors`
  [m old-key new-key]
  (update-key m old-key (constantly new-key)))

(tests
  (set-key nil :a :b)          := {:b nil}
  (set-key {} :a :b)           := {:b nil}
  (set-key nil :a :a)          := {:a nil}
  (set-key {:a 1} :a :b)       := {:b 1}
  (set-key {:a 1, :b 2} :a :c) := {:c 1, :b 2}
  )

(tests
  (update-key nil 1 inc) := {2 nil}
  (update-key {} 1 inc) := {2 nil}
  (update-key {1 :one} 1 inc) := {2 :one}
  (update-key '{(foo 1) bar} '(foo 1) (partial cons 'baz)) := '{(baz foo 1) bar}
  )
