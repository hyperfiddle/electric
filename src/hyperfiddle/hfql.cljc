(ns hyperfiddle.hfql
  (:require #?(:clj [hyperfiddle.hfql.impl :as impl])
            [hyperfiddle.photon :as p]
            [hyperfiddle.api :as-alias hf]
            [clojure.spec.alpha :as s])
  #?(:cljs (:require-macros [hyperfiddle.hfql])))

(defmacro precompile
  ([form] (impl/precompile* &env [] form))
  ([bindings form] (impl/precompile* &env bindings form)))

(defmacro hfql ; Alias for hfql macro in hfql ns
  ([query] `(new hf/Render (precompile ~query)))
  ([bindings query] `(new hf/Render (precompile ~bindings ~query))))

(p/defn JoinArg [ctx-or-V]
  (if (map? ctx-or-V)
    (JoinAllTheTree. ctx-or-V)
    (new ctx-or-V)))

(defn literal [collf & args] (collf args))    ; TODO rename

(s/fdef literal :args (s/cat :collf fn? :args any?) :ret any?)


(p/def Rec)

;; TODO Rename
(p/defn JoinAllTheTree "Join all the tree, does not call renderers, return EDN." [V]
  (binding [Rec (p/fn [{::hf/keys [type keys Value values]}]
                  (case type
                    ::hf/leaf (Value.)
                    ::hf/keys (into {} (zipmap keys (p/for [ctx values] (Rec. ctx))))
                    (let [ctx (Value.)]
                      (cond
                        (vector? ctx) (p/for [ctx ctx] (Rec. ctx))
                        (map? ctx)    (Rec. ctx)
                        :else         ctx))))]
    (new Rec V)))

;; TODO Rename, this seems to just be "Render"
(p/defn EdnRender "Join all the tree, calling renderers when provided, return EDN" [V]
  (binding [Rec (p/fn [{::hf/keys [type render keys Value values] :as ctx}]
                  (if render (render. ctx)
                      (case type
                        ::hf/leaf (Value.)
                        ::hf/keys (into {} (zipmap keys (p/for [ctx values] (Rec. ctx))))
                        (let [ctx (Value.)]
                          (cond
                            (vector? ctx) (p/for [ctx ctx] (Rec. ctx))
                            (map? ctx)    (Rec. ctx)
                            :else         ctx)))))]
    (new Rec V)))

(p/def Render JoinAllTheTree)
