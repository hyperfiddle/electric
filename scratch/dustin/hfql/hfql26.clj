;; #+TODO: TODO(t!) STARTED(s!) VERIFY(v!) | DONE(!) CANCELED(@/!)
;;
;; * HFQL transfer at every layer
;;
;;   Same as HFQL25 but add hf/render
;;
;; ** STARTED add hf/render defaulting to transfer identity
;;
;;    - State "STARTED"    from "TODO"       [2021-04-22 jeu. 15:33]
;;    #+begin_src clojure :eval never
;;    @(reactive-for (fn [%]
;;                     (dataflow
;;                      (hf/render …
;;                       (let [% %]
;;                         {:db/ident …})))))
;;    (defmethod hf/render :default [val props]
;;      (dataflow #_! val))
;;    #+end_src
;;
;;
;;    Up next:
;;    - [ ] check/review impl,
;;    - [ ] agree on what continue/render should dispatch on,
;;    - [ ] make transfer `!` work.
;;

(ns dustin.hfql.hfql26
  (:require [dustin.fiddle :refer [genders shirt-sizes submissions]]
            [dustin.hfql.hfql25 :refer [compile-leaf* hf-edge->sym! many? qualify replace*]]
            [hfdl.lang :refer [dataflow debug! result]]
            [hfdl.lib :refer [reactive-for]]
            [minitest :refer [tests]]
            [missionary.core :as m]))

(defmulti continue (fn [path _val] path))
(defmethod continue :default [_ val]
  (dataflow #_! val))

(defn compile-hfql*
  "compile HFQL form to s-expressions in Incremental"
  [env& ns-map env' path form]
  (cond
    (sequential? form)
    (apply merge (map (partial compile-hfql* env& ns-map env' path) form))

    (map? form)
    (reduce-kv (fn [r edge cont]
                 (let [qedge           (qualify env& ns-map edge)
                       path            (conj path qedge)
                       edge*           (compile-leaf* edge)
                       [env' edge-sym] (hf-edge->sym! env' edge)]
                   (merge r
                          (if (many? qedge)
                            `{'~qedge
                              @(reactive-for (~'fn [~'%]
                                              (dataflow
                                               @(continue '~path (~'let [~edge-sym ~'%]
                                                                  ~(compile-hfql* env& ns-map env' path cont)))))
                                             (~'unquote ~(replace* env' edge*)))}
                            `{'~qedge
                              (~'let [~'% ~(replace* env' edge*)]
                               @(continue '~path
                                          (~'let [~edge-sym ~'%]
                                           ~(compile-hfql* env& ns-map env' path cont))))}))))
               {}
               form)
    :else (let [qedge (qualify env& ns-map form)]
            {`'~qedge `@(continue '~(conj path qedge) ~(compile-leaf* form))})))

(defmacro hfql [form]
  (compile-hfql* &env (ns-map *ns*) {} [] form))

(tests
 (macroexpand-1 '(hfql {(genders) [:db/ident]}))
 :=
 '{'(dustin.fiddle/genders)
   @(hfdl.lib/reactive-for
     (fn
       [%]
       (hfdl.lang/dataflow
        @(dustin.hfql.hfql26/continue
          '[(dustin.fiddle/genders)]
          (let
              [% %]
            {':db/ident
             @(dustin.hfql.hfql26/continue
               '[(dustin.fiddle/genders) :db/ident]
               (dustin.hfql.hfql25/hf-nav :db/ident %))}))))
     (unquote (genders)))})


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
              #:db{:ident :dustingetz/female}]})

