;; #+TODO: TODO(t!) STARTED(s!) VERIFY(v!) | DONE(!) CANCELED(@/!)
;;
;;* HFQL which doesn't join anything
;;
;;  Same as [[file:~/Documents/fabric/scratch/geoffrey/hfql/links.clj::(ns geoffrey.hfql.links][HFQL with links]] but renderers have to sample streams on their own.

(ns geoffrey.hfql.no-join
  (:require [datascript.core :as d]
            [dustin.hfql.hfql25 :refer [hf-edge->sym! many? qualify replace*]]
            [hfdl.lang :refer [dataflow]]
            [hfdl.lib :refer [reactive-for]]
            [hyperfiddle.api :as hf]
            [clojure.walk :as walk]))

(defn hf-nav [kf >ref]
  (dataflow (kf (d/entity hyperfiddle.api/*$* @>ref))))

(defn compile-leaf* [?form]
  (cond
    (keyword? ?form) `(hf-nav ~?form ~'%)
    (seq? ?form)     ?form))

(defmacro default-renderer [val props]
  (if-let [a (::hf/a props)]
    `(hf/->Link ~a ~val)
    val))

(defmacro render [>val props]
  (let [render-sym (::hf/render props `default-renderer)
        props      (dissoc props ::hf/render)]
    `(~render-sym @~>val ~props)))

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



(comment
  (clojure.tools.analyzer.jvm/macroexpand-all
   '(hfql [{(geoffrey.fiddle-effects/shirt-size :dustingetz/male) [:db/id]}
           {(geoffrey.fiddle-effects/genders) [:db/ident]}])
   (clojure.tools.analyzer.jvm/empty-env))

  (clojure.tools.analyzer.jvm/macroexpand-all
   '(hfql :db/id)
   (clojure.tools.analyzer.jvm/empty-env))


  (defn custom-renderer [v props]
    (dataflow
     (let [id (get v :db/id)]
       `[:div ~props ~id])))

  (clojure.tools.analyzer.jvm/macroexpand-all
   '(hfql
     {((geoffrey.fiddle-effects/shirt-size :dustingetz/male) ::hf/render custom-renderer) [:db/id]})
   (clojure.tools.analyzer.jvm/empty-env))

  (def dag (dataflow (let [x @(hfql {((geoffrey.fiddle-effects/shirt-size :dustingetz/male) ::hf/render custom-renderer) [:db/id]})]
                       @(get x '(geoffrey.fiddle-effects/shirt-size :dustingetz/male)))))

  ((hfdl.lang/local2 (hfdl.lang/vars apply
                                     concat
                                     custom-renderer
                                     datascript.core/entity
                                     datascript.core/q
                                     first
                                     geoffrey.fiddle-effects/needle-rule
                                     geoffrey.fiddle-effects/shirt-size
                                     geoffrey.fiddle-effects/shirt-sizes
                                     get
                                     hash-map
                                     hf-nav
                                     hyperfiddle.api/*$*
                                     list
                                     seq
                                     sort
                                     vector)
                     (hfdl.lang/debug sampler dag)) prn prn)
  @sampler

  (render 1 {})  )
