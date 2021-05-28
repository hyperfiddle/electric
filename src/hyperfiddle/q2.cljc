(ns hyperfiddle.q2
  (:require [clojure.walk :as walk]
            [datascript.core :as d]
            [geoffrey.fiddle-effects :refer [genders shirt-sizes submissions submission submission-details]]
            [hfdl.lang :refer [#?(:clj vars) #?(:clj dataflow) #?(:clj debug) system]]
            [hfdl.lib :refer [reactive-for]]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hfdl.lang :refer [dataflow]])))

(defn hf-nav [kf >ref]
  #?(:cljs
     (dataflow (kf (d/entity hyperfiddle.api/*$* @>ref)))))

(defn replace* [smap coll]
  (if (seqable? coll)
    (replace smap coll)
    coll))

(defn compile-leaf* [?form]
  (cond
    (keyword? ?form) `(hf-nav ~?form ~'%)
    (seq? ?form)     ?form))

(defn map-entry [[k >v]]
  #?(:cljs (dataflow [k @>v])))

(defn join-1
  "Flow<{k, Flow<v>}> -> Flow<{k, v}>"
  [>map]
  #?(:cljs (dataflow (into {} @(reactive-for map-entry ~(seq @>map))))))

(defn default-renderer [>val props] ; m a -> m b
  #?(:cljs
     (if-let [a (::hf/a props)]
       (dataflow (hf/->Link a @>val))
       >val                                                 ; identity
       )))

(defn render [>val props]
  (let [render-sym (::hf/render props default-renderer)
        props      (dissoc props ::hf/render)]
    (render-sym >val props)))

(defn has-props? [form]
  (and (sequential? form)
       (or (keyword? (first form))
           (sequential? (first form)))))

(def hfql-prop? #{::hf/options ::hf/a})

(defn extract-props [form]
  (let [[form & {:keys [] :as props}] form]
    (if (sequential? form)
      (if (and (keyword? (first form))
               (or (= 1 (count form))
                   (= 2 (count form))))
        [(first form) props]
        [form props])
      [form props])))

(defn qualify [env& ns-map form]
  (walk/prewalk (fn [x]
                  (if (and (simple-symbol? x)
                           (nil? (get env& x)))
                    (if-let [?var (get ns-map x)]
                      (if (var? ?var) ; lexically bound if not a var
                        (symbol ?var)
                        x)
                      x)
                    x))
                form))

(defn qualify-link [env& ns-map form]
  (let [[f & args] form]
    (cons `list (qualify env& ns-map (cons (list 'quote f) args)))))

(declare compile-hfql*)

(defn expand-props [env& ns-map env' props]
  (reduce-kv (fn [r k v]
               (if (hfql-prop? k)
                 (case k
                   ::hf/a (assoc r k (qualify-link env& ns-map v))
                   (assoc r k (second (val (first (compile-hfql* env& ns-map env' v))))))
                 r))
             props props))

(defn quote* [env& env' xs]
  (walk/postwalk (fn [x]
                   (cond
                     (list? x)    (cons `list x)
                     (get env' x) (get env' x)
                     (get env& x) x
                     (keyword? x) x
                     :else        (list 'quote x)))
                 xs))

(def cardinality* {`submissions :db.cardinality/many
                   `shirt-sizes :db.cardinality/many
                   `genders     :db.cardinality/many})

(defn cardinality [form]
  (cond
    (symbol? form) (get cardinality* form)
    (keyword? form) (get cardinality* form)
    (seq? form) (let [[f & args] form]
                  (get cardinality* f)
                  #_(:db/cardinality (meta f)))))

(tests
  (cardinality `(shirt-sizes _)) := :db.cardinality/many
  (cardinality `shirt-sizes) := :db.cardinality/many)

(defn many? [form] (= :db.cardinality/many (cardinality form)))

(tests
  (many? :dustingetz/gender) := false
  (many? `shirt-sizes) := true
  (many? `shirt-size) := false
  (many? `(shirt-sizes a)) := true
  (many? `(shirt-size a)) := false)

(defn drop-slash [kw-sym]
  (symbol (str (namespace kw-sym) "__" (name kw-sym))))

(defn hf-edge->sym! [env edge]
  (if-let [sym (get env edge)]
    sym
    (if (keyword? edge)
      (if (namespace edge)
        (let [sym (drop-slash edge)]
          [(assoc env (symbol edge) sym) sym])
        [env (symbol (name edge))])
      [env '%])))

(defn compile-hfql*
  [env& ns-map env' form]
  (cond
    (vector? form)
    `(unquote ~(apply merge (map (partial compile-hfql* env& ns-map env') form)))

    (map? form)
    (reduce-kv (fn [r edge cont]
                 (let [[edge props]    (if (has-props? edge) (extract-props edge) [edge nil])
                       qedge           (qualify env& ns-map edge)
                       edge*           (compile-leaf* edge)
                       [env' edge-sym] (hf-edge->sym! env' edge)
                       props           (expand-props env& ns-map env' props)]
                   (merge r
                          (if (many? qedge)
                            `{~(quote* env& env' qedge)
                              ;; @(render )
                              ;; rfor :: (a -> m b) -> m [a] -> m [b]
                              (reactive-for (~'fn [~'%]
                                             (dataflow
                                              (~'let [~edge-sym (~'unquote ~'%)]
                                               @(render
                                                 ~(compile-hfql* env& ns-map env' cont)
                                                 ~props))))
                                            ~(replace* env' edge*))
                              ;; nil
                              }
                            `{~(quote* env& env' qedge)
                              (~'let [~'% ~(replace* env' edge*)]
                               (~'let [~edge-sym ~'%]
                                (render
                                 ~(compile-hfql* env& ns-map env' cont)
                                 ~props)))}))))
               {}
               form)

    :else (let [[form props] (if (has-props? form) (extract-props form) [form nil])
                props        (expand-props env& ns-map env' props)
                qedge        (qualify env& ns-map form)]
            {`~(quote* env& env' qedge) `(render ~(compile-leaf* (replace* env' form)) ~props)})))

#?(:clj
   (defmacro hfql [form]
     (compile-hfql* &env (ns-map *ns*) {} (if-not (vector? form) [form] form))))

(def exports
  #?(:clj
     (merge geoffrey.fiddle-effects/exports
       (vars default-renderer render
         list concat seq d/entity
         hfdl.lib/reactive-for
         join-1
         hf-nav hf/->Link))))

#_(let [needle @>needle
      x      @(hfql {(submission needle) [:dustingetz/email
                                          {:dustingetz/gender [:db/ident]}
                                          {:dustingetz/shirt-size [:db/ident]}]}) ]
  #_@(get x `(submission ""))
  x)

;; TODO test
(comment
  (def !needle (atom ""))
  (def >needle (m/watch !needle))

  (defn run-dag!
    ([dag] (run-dag! {} dag))
    ([vars dag]
     (def reactor ((system (merge exports (vars >needle) vars)
                           (debug sampler dag)) prn prn))
     @sampler))

  (run-dag! (dataflow @(hf-nav :db/id ~9)))
  := 9

  (run-dag! (dataflow (let [% ~9] (-> @(hfql :db/id)
                                      (:db/id)
                                      (deref)
                                      ))))
  := 9

  (run-dag! (dataflow (-> @(hfql {(submission "") [:db/id]})
                          (get '(geoffrey.fiddle-effects/submission ""))
                          (deref)
                          (:db/id)
                          (deref))))
  := 9

  (run-dag! (dataflow
             (-> @(hfql {(submission "") [{:dustingetz/shirt-size [:db/ident]}]})
                 (get '(geoffrey.fiddle-effects/submission ""))
                 (deref)
                 (:dustingetz/shirt-size)
                 (deref)
                 (:db/ident)
                 (deref))))
  := :dustingetz/womens-large

  ;; [X] 1. We should remove join points in hfql and get the same result because the default renderer samples by default. WRONG, the default renderer is identity Flow.

  ;; 2. Add custom renderers and sample (or not) manually.

  (defn id-as-string [>v props]
    (dataflow (str @>v)))

  (run-dag! (vars id-as-string str join-1)
            (dataflow (-> (hfql {(submission "") [:dustingetz/email (:db/id ::hf/render id-as-string)]})
                          (join-1)
                          (deref)
                          (get '(geoffrey.fiddle-effects/submission ""))
                          (:db/id)
                          (deref)
                          )))
  := "9"

  (run-dag! (vars join-1) (dataflow (let [% ~9] @(join-1 (hfql :db/id)))))
  := {:db/id 9}

  (run-dag! (vars id-as-string str join-1)
            (dataflow (-> (hfql {(submissions "") [:db/id]})
                          (deref)
                          (get '(geoffrey.fiddle-effects/submissions ""))
                          (deref)
                          (first)
                          (:db/id)
                          (deref))))
  := 9

  ;; 3. Call into UI library.

  ;; 4. Ensure we get UI + EDN (for testing)

  ;; 5. Put a flag on it.
  )


