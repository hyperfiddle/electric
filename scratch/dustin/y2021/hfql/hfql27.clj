;; #+TODO: TODO(t!) STARTED(s!) VERIFY(v!) | DONE(!) CANCELED(@/!)
;;
;;* 27. HF props support
;;
;;  Same as HFQL 26 but allow for `(edge ::hf/… props )` syntax.
;;
;;** DONE Add syntax support for props notation
;;   CLOSED: [2021-04-25 dim. 18:36]
;;
;;   - State "DONE"       from "STARTED"    [2021-04-25 dim. 18:36]
;;   - State "STARTED"    from "TODO"       [2021-04-25 dim. 09:10]
;;   - State "TODO"       from              [2021-04-25 dim. 09:06]
;;
;;   Last test doesn't pass because of a scope issue
;;
;;   `(shirt-sizes (:dustingetz/gender %))` is failing because in the current
;;   scope, `%` is 9 (Alice's id) since branches are parallel. We need to
;;   sort-out parallelism.

(ns dustin.hfql.hfql27
  (:require [dustin.fiddle :refer [genders shirt-sizes submissions]]
            [dustin.hfql.hfql25 :refer [compile-leaf* hf-edge->sym! many? qualify replace*]]
            [hfdl.lang :refer [dataflow debug! result]]
            [hfdl.lib :refer [reactive-for]]
            [minitest :refer [tests]]
            [missionary.core :as m]
            [datascript.core :as d]))

(defn default-renderer [val _props]
  (dataflow val))

(defn continue [val props]
  (let [renderf (::render props default-renderer)]
    (renderf val (dissoc props ::render))))

(defn has-props? [form]
  (and (sequential? form)
       (or (keyword? (first form))
           (sequential? (first form)))))

(def hfql-prop? #{::options})

(defn extract-props [form]
  (let [[form & {:keys [] :as props}] form]
    (if (sequential? form)
      (if (and (keyword? (first form))
               (or (= 1 (count form))
                   (= 2 (count form))))
        [(first form) props]
        [form props])
      [form props])))

(declare compile-hfql*)

(defn expand-props [env& ns-map env' props]
  (reduce-kv (fn [r k v]
               (if (hfql-prop? k)
                 (assoc r k (val (first (compile-hfql* env& ns-map env' v))))
                 r))
             props props) )

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
                            `{'~qedge
                              @(continue
                                @(reactive-for (~'fn [~'%]
                                                (dataflow
                                                 @(continue
                                                   (~'let [~edge-sym ~'%]
                                                    ~(compile-hfql* env& ns-map env' cont))
                                                   nil)))
                                               (~'unquote ~(replace* env' edge*)))
                                ~props)}
                            `{'~qedge
                              @(continue
                                (~'let [~'% ~(replace* env' edge*)]
                                 (~'let [~edge-sym ~'%]
                                  ~(compile-hfql* env& ns-map env' cont)))
                                ~props)}))))
               {}
               form)

    :else (let [[form props] (if (has-props? form) (extract-props form) [form nil])
                props        (expand-props env& ns-map env' props)
                qedge        (qualify env& ns-map form)]
            {`'~qedge `@(continue ~(compile-leaf* form) ~props)})))

(defmacro hfql [form]
  (compile-hfql* &env (ns-map *ns*) {} form))

(tests
 (macroexpand-1 '(hfql (genders)))
 :=
 '{'(dustin.fiddle/genders) @(dustin.hfql.hfql27/continue (genders) nil)})

(defn ul [val props]
  (dataflow
   (into [:ul props] (map :db/ident val))))

(defn li [val props]
  (dataflow
   [:li props val]))

(tests
 (def program (dataflow (hfql {((genders) ::render ul :title "Genders") [(:db/ident ::render li)]})))
 (def process (debug! program))
 (result program @process)
 := `{(genders) [:ul {:title "Genders"}
                 [:li {} :dustingetz/male]
                 [:li {} :dustingetz/female]]}
 )

(defn simple-picklist [value {:keys [::options] :as props}]
  (dataflow
   (into [:select {:value value}]
         (for [option options]
           [:option (:db/ident option)]))))

(tests

 (def !needle (atom ""))
 (def >needle (m/watch !needle))
 (def program (dataflow
               (let [needle @>needle]
                 (hfql {(submissions needle) [:dustingetz/email
                                              {(:dustingetz/gender ::options (genders)) [:db/ident]}
                                              {(:dustingetz/shirt-size ::options (shirt-sizes (:dustingetz/gender %))
                                                                       ::render simple-picklist)
                                               [:db/ident]}]}))))

 (def process (debug! program))

 (result program @process)
 :=
 `{(submissions ~'needle)
   [#:dustingetz{:email "alice@example.com",
                 :gender #:db{:ident :dustingetz/female},
                 :shirt-size
                 [:select {:value #:db{:ident :dustingetz/womens-large}}
                  [:option :dustingetz/womens-small]
                  [:option :dustingetz/womens-medium]
                  [:option :dustingetz/womens-large]]}
    #:dustingetz{:email "bob@example.com",
                 :gender #:db{:ident :dustingetz/male},
                 :shirt-size
                 [:select {:value #:db{:ident :dustingetz/mens-large}}
                  [:option :dustingetz/mens-small]
                  [:option :dustingetz/mens-medium]
                  [:option :dustingetz/mens-large]]}
    #:dustingetz{:email "charlie@example.com",
                 :gender #:db{:ident :dustingetz/male},
                 :shirt-size
                 [:select {:value #:db{:ident :dustingetz/mens-medium}}
                  [:option :dustingetz/mens-small]
                  [:option :dustingetz/mens-medium]
                  [:option :dustingetz/mens-large]]}]}
 )
