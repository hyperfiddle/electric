(ns hyperfiddle.q2
  (:require [clojure.walk :as walk]
            [datascript.core :as d]
            [geoffrey.fiddle-effects :refer [genders shirt-sizes submissions submission submission-details]]
            [hfdl.lang :refer [dataflow system debug vars]]
            [hfdl.lib :refer [reactive-for]]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

(defn hf-nav [kf >ref]
  (dataflow (kf (d/entity hyperfiddle.api/*$* @>ref))))

(defn replace* [smap coll]
  (if (seqable? coll)
    (replace smap coll)
    coll))

(defn compile-leaf* [?form]
  (cond
    (keyword? ?form) `(hf-nav ~?form ~'%)
    (seq? ?form)     ?form))

(defn default-renderer [val props]
  (if-let [a (::hf/a props)]
    (dataflow (hf/->Link a @val))
    val ; identity
    ))

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
                              @(reactive-for (~'fn [~'%]
                                              (dataflow
                                               (~'let [~edge-sym ~'%]
                                                (render
                                                 ~(compile-hfql* env& ns-map env' cont)
                                                 ~props))))
                                             (~'unquote ~(replace* env' edge*)))
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

(defmacro hfql [form]
  (compile-hfql* &env (ns-map *ns*) {} (if-not (vector? form) [form] form)))

(def exports
  (merge geoffrey.fiddle-effects/exports
         hfdl.lang/exports
         (vars default-renderer render
               list concat seq d/entity
               hfdl.lib/reactive-for
               hf-nav hf/->Link)))

#_(let [needle @>needle
      x      @(hfql {(submission needle) [:dustingetz/email
                                          {:dustingetz/gender [:db/ident]}
                                          {:dustingetz/shirt-size [:db/ident]}]}) ]
  #_@(get x `(submission ""))
  x)

(tests
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

  (run-dag! (vars id-as-string str) (dataflow (-> @(hfql {(submission "") [:dustingetz/email (:db/id ::hf/render id-as-string)]})
                                              (get '(geoffrey.fiddle-effects/submission ""))
                                              (deref)
                                              (:db/id)
                                              (deref))))
  := "9"


  (defn join-all [>map]
    (dataflow (into {} @(reactive-for (fn [[k >v]]
                                        (dataflow [k @>v]))
                                      ~(seq @>map)))))

  (run-dag! (vars join-all) (dataflow (let [% ~9] @(join-all (hfql :db/id)))))
  := {:db/id 9}

  ;; 3. Call into UI library.

  ;; 4. Ensure we get UI + EDN (for testing)

  ;; 5. Put a flag on it.
  )


