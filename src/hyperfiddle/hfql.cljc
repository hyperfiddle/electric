(ns hyperfiddle.hfql
  (:require #?(:clj [hyperfiddle.hfql.impl :as impl])
            [hyperfiddle.photon :as p]
            [missionary.core :as m]
            #_[hyperfiddle.api :as-alias hf] ; cursive can't handle this, and a close read of the docs say this is still a cycle if it exists
            [clojure.spec.alpha :as s])
  #?(:cljs (:require-macros [hyperfiddle.hfql])))

(defmacro precompile
  ([form] (impl/precompile* &env [] form))
  ([bindings form] (impl/precompile* &env bindings form)))

(defmacro hfql ; Alias for hfql macro in hfql ns
  ([query] `(new hyperfiddle.api/Render (precompile ~query)))
  ([bindings query] `(new hyperfiddle.api/Render (precompile ~bindings ~query))))

(p/defn JoinArg [ctx-or-V]
  (if (map? ctx-or-V)
    (JoinAllTheTree. ctx-or-V)
    (new ctx-or-V)))

(defn literal [collf & args] (collf args))    ; TODO rename

(s/fdef literal :args (s/cat :collf fn? :args any?) :ret any?)


(p/def Rec)

;; TODO Rename
(p/defn JoinAllTheTree "Join all the tree, does not call renderers, return EDN." [V]
  (binding [Rec (p/fn [{:hyperfiddle.api/keys [type keys Value values]}]
                  (case type
                    :hyperfiddle.api/leaf (Value.)
                    :hyperfiddle.api/keys (into {} (zipmap keys (p/for [ctx values] (Rec. ctx))))
                    (let [ctx (Value.)]
                      (cond
                        (vector? ctx) (p/for [ctx ctx] (Rec. ctx))
                        (map? ctx)    (Rec. ctx)
                        :else         ctx))))]
    (new Rec V)))

;; TODO Rename, this seems to just be "Render"
(p/defn EdnRender "Join all the tree, calling renderers when provided, return EDN" [V]
  (binding [Rec (p/fn [{:hyperfiddle.api/keys [type render keys Value values] :as ctx}]
                  (if render (render. ctx)
                      (case type
                        :hyperfiddle.api/leaf (Value.)
                        :hyperfiddle.api/keys (into {} (zipmap keys (p/for [ctx values] (Rec. ctx))))
                        (let [ctx (Value.)]
                          (cond
                            (vector? ctx) (p/for [ctx ctx] (Rec. ctx))
                            (map? ctx)    (Rec. ctx)
                            :else         ctx)))))]
    (new Rec V)))

(p/def Render JoinAllTheTree)

;; FIXME this is just m/signal in next reactor iteration
(defmacro ^:no-doc share [init flow] ; HACK init value should not be needed
  `(let [!subscribers# (atom #{})
         !value#       (atom ~init)
         auto-inc#     (partial swap! (atom 0) inc)]
     (when (seq (p/watch !subscribers#))
       (reset! !value# (new ~flow))) ; FIXME exceptions thrown here won’t propagate where the shared flow was joined but where it was shared.
     (p/fn []
       (new (m/observe (fn [!#]
                         (let [subscriber-id# (auto-inc#)]
                           (swap! !subscribers# conj subscriber-id#)
                           (!# nil)
                           #(swap! !subscribers# disj subscriber-id#)))))
       (p/watch !value#) ; this sees `init` for 1 frame
       )))

(comment
  (tests
    "Sharing a flow allow multiple subscribers"
    ;; The first `new` mounts the flow. The flow stays up as long as there is at least one subscriber.
    (def !state (atom 1))
    (def !control (atom true))
    (with (p/run (let [flow (share ::init (p/fn []
                                            (new (m/observe (fn [!]
                                                              (tap "up")
                                                              (! nil)
                                                              #(tap "down"))))
                                            (p/watch !state)
                                            ))]
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

