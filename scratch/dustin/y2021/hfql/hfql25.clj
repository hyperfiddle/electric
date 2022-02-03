;; * Rewrite of HFQL 24 to qualify or unqualify symbols
;;
;;   Same tests, same context as HFQL 24. Only `compile-hfql*` changes.
;;
;; ** DONE unqualify in let binding
;;    CLOSED: [2021-04-22 jeu. 12:10]
;;    `clojure.core/let` don't support it. We need to generate a unique name
;;    compatible with `c.c/let` and rewrite terms where needed.
;;
;; ** DONE qualify symbols in quoted forms
;;    CLOSED: [2021-04-22 jeu. 15:27]
;;    According to current ns, including aliases, except for lexically bound
;;    ones.
;;
;;    We manually qualify quoted forms and let the clojure compiler deal with
;;    the non-quoted ones.

(ns dustin.hfql.hfql25
  (:require
   [datascript.core :as d]                                 ;#?(:clj [datomic.api :as d])
   [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
   [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]
   [hfdl.lang :refer [dataflow debug! result]]
   [hfdl.lib :refer [reactive-for]]
   [minitest :refer [tests]]
   [missionary.core :as m]
   [clojure.walk :as walk]))

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

; entity navigation

(defn hf-nav [kf ref]
  ; emits smart refs
  (kf (d/entity hyperfiddle.api/*$* ref)))

(tests
  (:db/ident (d/touch (datascript.core/entity hyperfiddle.api/*$* 3)))
  := :dustingetz/mens-small

  (hf-nav :db/ident 3) := :dustingetz/mens-small
  (hf-nav :db/id 3) := 3
  ;(hf-nav identity [:dustingetz/email "alice@example.com"]) := #:db{:id 9}
  (hf-nav :db/id [:dustingetz/email "alice@example.com"]) := 9
  (hf-nav :dustingetz/gender [:dustingetz/email "alice@example.com"]) := :dustingetz/female)

; HFQL

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

(defn compile-leaf* [?form]
  (cond
    (keyword? ?form) `(hf-nav ~?form ~'%)
    (seq? ?form)     ?form))

(defn replace* [smap coll]
  (if (seqable? coll)
    (replace smap coll)
    coll))

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

(tests
  (qualify {'a 1} (ns-map *ns*) 'submissions) := `submissions
  (qualify {'a 1} (ns-map *ns*) 'a) := 'a
  )

(defn compile-hfql*
  "compile HFQL form to s-expressions in Incremental"
  [env& ns-map env' form]
  (cond
    (sequential? form)
    (apply merge (mapv (partial compile-hfql* env& ns-map env') form))

    (map? form)
    (reduce-kv (fn [r edge cont]
                 (let [qedge           (qualify env& ns-map edge)
                       edge*           (compile-leaf* edge)
                       [env' edge-sym] (hf-edge->sym! env' edge)]
                   (merge r
                          (if (many? qedge)
                            `{'~qedge
                              @(reactive-for (~'fn [~'%]
                                              (dataflow
                                               (~'let [~edge-sym ~'%]
                                                ~(compile-hfql* env& ns-map env' cont))))
                                             (~'unquote ~(replace* env' edge*)))}
                            `{'~qedge
                              (~'let [~'% ~(replace* env' edge*)]
                               (~'let [~edge-sym ~'%]
                                ~(compile-hfql* env& ns-map env' cont)))}))))
               {}
               form)
    :else {`'~(qualify env& ns-map form) (compile-leaf* form)}))

(defmacro hfql [form]
  (compile-hfql* &env (ns-map *ns*) {} form))

(tests
 (macroexpand-1 '(hfql {(genders) [:db/ident]}))
 := '{'(dustin.fiddle/genders)
      @(hfdl.lib/reactive-for
        (fn [%] (hfdl.lang/dataflow
                (let [% %] {':db/ident (dustin.hfql.hfql25/hf-nav :db/ident %)})))
        (unquote (genders)))})

(tests
 (def program (dataflow (hfql {(genders) [:db/ident]})))

 (def process (debug! program))
 (result program @process) := `{(genders) [{:db/ident :dustingetz/male}
                                           {:db/ident :dustingetz/female}]})

(tests
 (def !gender (atom :dustingetz/male))
 (def >gender (m/watch !gender))
 (def program (dataflow (let [gender @>gender]
                          (hfql {(shirt-size gender) [:db/id :db/ident]}))))
 (def process (debug! program))

 (result program @process) := '{(dustin.fiddle/shirt-size gender) {:db/id 3 :db/ident :dustingetz/mens-small}}

 )

(tests

 (def !needle (atom ""))
 (def >needle (m/watch !needle))
 (def program (dataflow
               (let [needle @>needle]
                 (hfql [{(submissions needle)
                         [:dustingetz/email
                          {:dustingetz/gender
                           [:db/ident
                            {(shirt-sizes dustingetz/gender) [:db/ident]}]}]}
                        {(genders) [:db/ident]}]))))

 (def process (debug! program))

 (result program @process)
 :=
 `{(submissions ~'needle)
   [#:dustingetz{:email "alice@example.com"
                 :gender
                 {(shirt-sizes dustingetz/gender)
                  [#:db{:ident :dustingetz/womens-small}
                   #:db{:ident :dustingetz/womens-medium}
                   #:db{:ident :dustingetz/womens-large}]
                  :db/ident :dustingetz/female}}
    #:dustingetz{:email "bob@example.com"
                 :gender
                 {(shirt-sizes dustingetz/gender)
                  [#:db{:ident :dustingetz/mens-small}
                   #:db{:ident :dustingetz/mens-medium}
                   #:db{:ident :dustingetz/mens-large}]
                  :db/ident :dustingetz/male}}
    #:dustingetz{:email "charlie@example.com"
                 :gender
                 {(shirt-sizes dustingetz/gender)
                  [#:db{:ident :dustingetz/mens-small}
                   #:db{:ident :dustingetz/mens-medium}
                   #:db{:ident :dustingetz/mens-large}]
                  :db/ident :dustingetz/male}}]
   (genders) [#:db{:ident :dustingetz/male}
              #:db{:ident :dustingetz/female}]}

 (reset! !needle "alice")

 (result program @process)
 :=
 `{(submissions ~'needle)
   [#:dustingetz{:email "alice@example.com"
                 :gender
                 {:db/ident :dustingetz/female
                  (shirt-sizes dustingetz/gender)
                  [#:db{:ident :dustingetz/womens-small}
                   #:db{:ident :dustingetz/womens-medium}
                   #:db{:ident :dustingetz/womens-large}]}}]
   (genders) [#:db{:ident :dustingetz/male}
              #:db{:ident :dustingetz/female}]}

 )
