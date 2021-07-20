(ns hyperfiddle.q
  (:require [clojure.walk :as walk]
            [datascript.core :as d]
            [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission submission-details]]
            [hfdl.lang :refer [dataflow local2 debug vars]]
            [hfdl.lib :refer [reactive-for]]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m]))

(defn default-renderer [val props]
  (prn 'default-renderer val props)
  (if-let [a (::hf/a props)]
    (dataflow (hf/->Link a @val)) ;; TODO
    (dataflow val)))

(defn render [val props]
  (let [renderf (::hf/render props default-renderer)]
    (renderf val (dissoc props ::hf/render))))

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
                   (assoc r k (val (first (compile-hfql* env& ns-map env' v)))))
                 r))
    props props))

(defn quote* [env& env' xs]
  (walk/postwalk (fn [x]
                   (cond
                     (list? x)    (cons `list x)
                     (get env' x) (get env' x)
                     (get env& x) x
                     :else        (list 'quote x)))
    xs))

(defn hf-nav [kf ref]
  ; emits smart refs
  (dataflow (kf (d/entity hyperfiddle.api/*$* ref))))

(defn compile-leaf* [?form]
  (cond
    (keyword? ?form) `(hf-nav ~?form ~'%)
    (seq? ?form)     ?form))

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

(defn many? [form] (= :db.cardinality/many (cardinality form)))

(defn replace* [smap coll]
  (if (seqable? coll)
    (replace smap coll)
    coll))

(defn compile-hfql*
  [env& ns-map env' form]
  (cond
    (vector? form)
    (apply merge (map (partial compile-hfql* env& ns-map env') form))

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
                              @(reactive-for (~'fn [~'%]
                                              (dataflow
                                               (~'let [~edge-sym ~'%]
                                                (render
                                                 ~(compile-hfql* env& ns-map env' cont)
                                                 ~props))))
                                             (~'unquote ~(replace* env' edge*)))
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
  (compile-hfql* &env (ns-map *ns*) {} form))

(def exports
  (vars with-meta hash-map list vector into get seq map
    reactive-for submissions shirt-sizes hf-nav render
    hf/->Link genders submission-details))
