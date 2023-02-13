;; #+TODO: TODO(t!) STARTED(s!) VERIFY(v!) | DONE(!) CANCELED(@/!)
;;
;;* 27. HF props support
;;
;;  Same as [[file:~/Documents/fabric/scratch/dustin/hfql/hfql27.clj::(ns dustin.hfql.hfql27][HFQL 27]]  but adds a way to render a link with `::hf/a` prop.
;;
;;
;;** DONE Make a textual link representation
;;   CLOSED: [2021-04-28 mer. 09:47]
;;
;;   - State "DONE"       from "TODO"       [2021-04-28 mer. 09:05]
;;   - State "TODO"       from              [2021-04-28 mer. 08:58]
;;   It will be a simple `pr-str` and `edn/read-string` for now.
;;
;;
;;** DONE Make the `::hf/a link-form` prop render a link
;;   CLOSED: [2021-04-28 mer. 10:12]
;;
;;   - State "DONE"       from "STARTED"    [2021-04-28 mer. 10:12]
;;   - State "STARTED"    from "TODO"       [2021-04-28 mer. 09:10]
;;   - State "TODO"       from              [2021-04-28 mer. 08:58]
;;   Syms (e.g. %) need to resolve from the HFQL env.
;;
;;   NOTE: HFQL syntax makes identities explicit, no need to figure out the
;;   "best" one.
;;
;;   Render an `:a` tag inline, not sure if it's the best way to customize
;;   rendering based on a prop.
;;
;;
;;** TODO Make a router
;;   - State "TODO"       from              [2021-04-28 mer. 08:58]
;;
;;     It might not be a URI router, but rather a functions whitelist.
;;
;;     When we will use URIs, we will need a regular router + a parser.
;;

(ns geoffrey.hfql.links
  (:require [dustin.fiddle :refer [genders shirt-sizes submissions submission-details]]
            [dustin.hfql.hfql25 :refer [compile-leaf* hf-edge->sym! many? qualify replace*]]
            [hfdl.lang :refer [dataflow]]
            [hfdl.lib :refer [reactive-for]]
            [hyperfiddle.api :as hf]
            [minitest :refer [tests]]
            [clojure.walk :as walk]))

(defn default-renderer [val props]
  (if-let [a (::hf/a props)]
    (dataflow (hf/->Link a val))
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
                              @(reactive-for (~'fn [~'%]
                                                         (dataflow
                                                          (~'let [~edge-sym ~'%]
                                                           @(render
                                                             ~(compile-hfql* env& ns-map env' cont)
                                                             ~props))))
                                                        (~'unquote ~(replace* env' edge*)))
                              ;; nil
                              }
                            `{~(quote* env& env' qedge)
                              (~'let [~'% ~(replace* env' edge*)]
                               (~'let [~edge-sym ~'%]
                                @(render
                                  ~(compile-hfql* env& ns-map env' cont)
                                  ~props)))}))))
               {}
               form)

    :else (let [[form props] (if (has-props? form) (extract-props form) [form nil])
                props        (expand-props env& ns-map env' props)
                qedge        (qualify env& ns-map form)]
            {`~(quote* env& env' qedge) `@(render ~(compile-leaf* (replace* env' form)) ~props)})))

(defmacro hfql [form]
  (compile-hfql* &env (ns-map *ns*) {} form))

#_(tests

 (macroexpand-1 '(hfql {(submissions needle) [(:db/id ::hf/a (submission-details %))]}))

 (def program (dataflow
               (let [needle ""]
                 (hfql {(submissions needle) [(:db/id ::hf/a (submission-details %))]}))))
 (def process (debug! program))
 (result program @process)
 := {'(dustin.fiddle/submissions needle)
     [#:db{:id (hf/->Link '(dustin.fiddle/submission-details 9) 9)}
      #:db{:id (hf/->Link '(dustin.fiddle/submission-details 10) 10)}
      #:db{:id (hf/->Link '(dustin.fiddle/submission-details 11) 11)}]}
 )

#_(tests

 (def !needle (atom ""))
 (def >needle (missionary.core/watch !needle))
 (def program (dataflow
               (let [needle @>needle]
                 (hfql {(submissions needle) [:dustingetz/email
                                              {(:dustingetz/gender ::hf/options (genders)) [:db/ident]}
                                              {(:dustingetz/shirt-size #_#_::hf/options (shirt-sizes (:dustingetz/gender %))
                                                                       #_#_::hf/render simple-picklist)
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
