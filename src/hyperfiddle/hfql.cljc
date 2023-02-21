(ns hyperfiddle.hfql
  (:require #?(:clj [hyperfiddle.hfql-compiler :as impl])
            [hyperfiddle.electric :as p]
            [missionary.core :as m]
            #_[hyperfiddle.api :as-alias hf] ; cursive can't handle this, and a close read of the docs say this is still a cycle if it exists
            [clojure.spec.alpha :as s]
            [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
            [hyperfiddle.history])
  #?(:cljs (:require-macros [hyperfiddle.hfql])))

(defmacro precompile
  ([form] (impl/precompile* &env [] form nil))
  ([bindings form] (impl/precompile* &env bindings form nil))
  ([bindings form eid] (impl/precompile* &env bindings form eid)))

(defmacro hfql ; Alias for hfql macro in hfql ns
  ([query] `(new hyperfiddle.api/Render (precompile ~query)))
  ([bindings query] `(new hyperfiddle.api/Render (precompile ~bindings ~query)))
  ([bindings query eid] `(new hyperfiddle.api/Render (precompile ~bindings ~query ~eid))))

(p/defn JoinArg [ctx-or-V]
  (if (map? ctx-or-V)
    (JoinAllTheTree. ctx-or-V)
    (new ctx-or-V)))

(defn literal [collf & args] (collf args))    ; TODO rename

(s/fdef literal :args (s/cat :collf fn? :args any?) :ret any?)


(p/def Rec)

(defn -drop-wildcards "Given a context, build a sequence of key-value pairs, ignoring any wildcard"
  [{:hyperfiddle.api/keys [keys values]}]
  (remove (fn [[k _v]] (= '_ k)) (partition 2 (interleave keys values))))

;; TODO Rename
(p/defn JoinAllTheTree "Join all the tree, does not call renderers, return EDN." [V]
  (binding [Rec (p/fn [{:hyperfiddle.api/keys [type Value] :as ctx}]
                  (case type
                    :hyperfiddle.api/leaf (Value.)
                    :hyperfiddle.api/keys (into {} (p/for [[k ctx] (-drop-wildcards ctx)]
                                                     [k (Rec. ctx)]))
                    (if Value
                      (let [ctx (Value.)]
                        (cond
                          (vector? ctx) (p/for [ctx ctx] (Rec. ctx))
                          (map? ctx)    (Rec. ctx)
                          :else         ctx))
                      ctx)))]
    (new Rec V)))

;; TODO Rename, this seems to just be "Render"
(p/defn EdnRender "Join all the tree, calling renderers when provided, return EDN" [V]
  (binding [Rec (p/fn [{:hyperfiddle.api/keys [type render Value] :as ctx}]
                  (if render (render. ctx)
                      (case type
                        :hyperfiddle.api/leaf (Value.)
                        :hyperfiddle.api/keys (into {} (p/for [[k ctx] (-drop-wildcards ctx)]
                                                         [k (Rec. ctx)]))
                        (let [ctx (Value.)]
                          (cond
                            (vector? ctx) (p/for [ctx ctx] (Rec. ctx))
                            (map? ctx)    (Rec. ctx)
                            :else         ctx)))))]
    (new Rec V)))

(p/def Render JoinAllTheTree)

;; FIXME this is just m/signal in next reactor iteration
#_(p/defn ^:no-doc Share [init flow]  ; HACK init value should not be needed
  (let [!subscribers (atom #{})
        !value       (atom init)
        !err         (atom nil)
        auto-inc     (partial swap! (atom 0) inc)]
    (when (seq (p/watch !subscribers))
      (try (when-some [v (new flow)]
             (reset! !value v)
             (reset! !err nil))
           (catch Throwable t         ; Convey errors to subscribers
             (reset! !err t))))
    (p/fn []
      (new (m/observe (fn [!]
                        (let [subscriber-id (auto-inc)]
                          (swap! !subscribers conj subscriber-id)
                          (! nil)
                          #(swap! !subscribers disj subscriber-id)))))
      (if-some [err (p/watch !err)]
        (throw err)
        (p/watch !value)) ; this sees `init` for 1 frame
      )))

;; (p/defn Share [init flow] flow)
(defmacro share [init flow] flow) ; temporary stub

(comment
  (tests
    "Sharing a flow allow multiple subscribers"
    ;; The first `new` mounts the flow. The flow stays up as long as there is at least one subscriber.
    (def !state (atom 1))
    (def !control (atom true))
    (with (p/run (let [flow (Share. ::init (p/fn []
                                             (new (m/observe (fn [!]
                                                               (tap "up")
                                                               (! nil)
                                                               #(tap "down"))))
                                             (p/watch !state)))]
                   (when (p/watch !control)
                     (tap (new flow)))
                   (tap (new flow)))
            )
      % := ::init ; HACK
      % := ::init ; HACK
      % := "up"
      % := 1
      % := 1
      (swap! !control not)
      (swap! !state inc)
      % := 2)
    % := "down"
    ))


(defn safe-count [xs] (if (counted? xs) (count xs) 0))
