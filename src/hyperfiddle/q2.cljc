(ns hyperfiddle.q2
  (:require [clojure.walk :as walk]
            [datascript.core :as d]
            [hfdl.lang :refer [vars #?(:clj defnode) debug system] :as h]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :refer [tests]]
            [missionary.core :as m])
  #?(:cljs (:require-macros
             [hfdl.lang :refer [defnode]]
             [hyperfiddle.q2 :refer [hfql]])))

(defn hf-nav* [db kf ref]
  (m/ap (m/? (m/via m/blk (kf (d/entity db ref))))))

(defnode hf-nav [kf ref]
  ~(hf-nav* hf/*$* kf ref))

(defn replace* [smap coll]
  (if (seqable? coll)
    (replace smap coll)
    coll))

(defn compile-leaf* [?form]
  (cond
    (keyword? ?form) `(hf-nav ~?form ~'%)
    (seq? ?form)     ?form))

(defmacro default-renderer [val props]
  (if-let [a (::hf/a props)]
    (list `hf/->Link a val) val))

(defmacro render [val props]
  (list (::hf/render props `default-renderer) val (dissoc props ::hf/render)))

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

; todo look at spec
(def cardinality* {'user.gender-shirt-size/submissions :db.cardinality/many
                   'user.gender-shirt-size/shirt-sizes :db.cardinality/many
                   'user.gender-shirt-size/genders     :db.cardinality/many})

(defn cardinality [form]
  (cond
    (symbol? form) (get cardinality* form)
    (keyword? form) (get cardinality* form)
    (seq? form) (let [[f & args] form]
                  (get cardinality* f)
                  #_(:db/cardinality (meta f)))))

(tests
  (cardinality '(user.gender-shirt-size/shirt-sizes _ nil)) := :db.cardinality/many
  (cardinality 'user.gender-shirt-size/shirt-sizes) := :db.cardinality/many)

(defn many? [form] (= :db.cardinality/many (cardinality form)))

(tests
  (many? :dustingetz/gender) := false
  (many? 'user.gender-shirt-size/shirt-sizes) := true
  (many? 'user.gender-shirt-size/shirt-size) := false
  (many? '(user.gender-shirt-size/shirt-sizes a nil)) := true
  (many? '(user.gender-shirt-size/shirt-size a)) := false)

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
                              ;; @(render )
                              ;; rfor :: (a -> m b) -> m [a] -> m [b]
                              (h/for [~'% ~(replace* env' edge*)]
                                (let [~edge-sym ~'%]
                                  (render ~(compile-hfql* env& ns-map env' cont) ~props)))
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
  (merge user.gender-shirt-size/exports
         (vars hash-map vector list concat seq hf-nav* hf/->Link)))

(tests
  (macroexpand '(hfql :db/id)) :=
  '#:db{:id (hyperfiddle.q2/render (hyperfiddle.q2/hf-nav :db/id %) nil)}

  (macroexpand '(hfql {(user.gender-shirt-size/submission "") [:db/id]})) :=
  '{(clojure.core/list (quote user.gender-shirt-size/submission) (quote ""))
   (let [% (user.gender-shirt-size/submission "")]
     (let [% %]
       (hyperfiddle.q2/render
         #:db{:id (hyperfiddle.q2/render
                    (hyperfiddle.q2/hf-nav :db/id %)
                    nil)} nil)))}

  (macroexpand '(hfql {(user.gender-shirt-size/submission "") [{:dustingetz/shirt-size [:db/ident]}]})) :=
  '{(clojure.core/list (quote user.gender-shirt-size/submission) (quote ""))
   (let [% (user.gender-shirt-size/submission "")]
     (let [% %]
       (hyperfiddle.q2/render
         #:dustingetz{:shirt-size (let [% (hyperfiddle.q2/hf-nav :dustingetz/shirt-size %)]
                                    (let [dustingetz__shirt-size %]
                                      (hyperfiddle.q2/render
                                        #:db{:ident (hyperfiddle.q2/render
                                                      (hyperfiddle.q2/hf-nav
                                                        :db/ident %)
                                                      nil)} nil)))} nil)))}

  (macroexpand '(hfql {(user.gender-shirt-size/submission "") [:dustingetz/email (:db/id ::hf/render id-as-string)]})) :=
  '{(clojure.core/list (quote user.gender-shirt-size/submission) (quote ""))
   (let [% (user.gender-shirt-size/submission "")]
     (let [% %]
       (hyperfiddle.q2/render
         {:dustingetz/email (hyperfiddle.q2/render (hyperfiddle.q2/hf-nav :dustingetz/email %) nil),
          :db/id (hyperfiddle.q2/render (hyperfiddle.q2/hf-nav :db/id %)
                   #:hyperfiddle.api{:render id-as-string})} nil)))}

  (macroexpand '(hfql {(user.gender-shirt-size/submissions "") [:db/id]})) :=
  '{(clojure.core/list (quote user.gender-shirt-size/submissions) (quote ""))
    (hfdl.lang/for [% (user.gender-shirt-size/submissions "")]
                   (clojure.core/let [% %]
                     (hyperfiddle.q2/render #:db{:id (hyperfiddle.q2/render (hyperfiddle.q2/hf-nav :db/id %)
                                                                            nil)} nil)))}

  )

;; TODO test
(comment
  (def !needle (atom ""))
  (def >needle (m/watch !needle))

  (def !res (atom nil))
  ((system exports (reset! !res (hf-nav :db/id 9))) nil nil)
  @!res := 9

  (reset! !res nil)
  ((system exports (reset! !res (let [% 9] (:db/id (hfql :db/id))))) nil nil)
  @!res := 9

  (reset! !res nil)
  ((system exports
     (reset! !res
       (-> (hfql {(submission "") [:db/id]})
         (get '(user.gender-shirt-size/submission ""))
         (:db/id)))) nil nil)
  @!res := 9

  (reset! !res nil)
  ((system exports
     (reset! !res
       (-> (hfql {(submission "") [{:dustingetz/shirt-size [:db/ident]}]})
         (get '(user.gender-shirt-size/submission ""))
         (:dustingetz/shirt-size)
         (:db/ident)))) nil nil)
  @!res := :dustingetz/womens-large

  ;; [X] 1. We should remove join points in hfql and get the same result because the default renderer samples by default. WRONG, the default renderer is identity Flow.

  ;; 2. Add custom renderers and sample (or not) manually.

  (defnode id-as-string [v props]
    (str v))

  (reset! !res nil)
  ((system exports
     (reset! !res
       (-> (hfql {(submission "") [:dustingetz/email (:db/id ::hf/render id-as-string)]})
         (get '(user.gender-shirt-size/submission ""))
         (:db/id)))) nil nil)
  @!res := "9"

  (reset! !res nil)
  ((system exports (reset! !res (let [% 9] (hfql :db/id)))) nil nil)
  @!res := {:db/id 9}

  (reset! !res nil)
  ((system exports
     (reset! !res
       (-> (hfql {(submissions "") [:db/id]})
         (get '(user.gender-shirt-size/submissions ""))
         (first)
         (:db/id)))) nil nil)
  @!res := 9

  ;; 3. Call into UI library.

  ;; 4. Ensure we get UI + EDN (for testing)

  ;; 5. Put a flag on it.
  )

