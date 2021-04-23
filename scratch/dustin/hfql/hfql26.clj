;; #+TODO: TODO(t!) STARTED(s!) VERIFY(v!) | DONE(!) CANCELED(@/!)
;;
;; * HFQL transfer at every layer
;;
;;   Same as HFQL25 but add hf/render (continue)
;;
;; ** DONE add hf/render defaulting to transfer identity
;;    CLOSED: [2021-04-23 ven. 09:04]
;;
;;    - State "DONE"       from "STARTED"    [2021-04-23 ven. 09:04]
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
;;    - [X] check/review impl: Feedback: `continue` is not re-entrant like
;;      hf/render should be, no way to stop drilling at an arbitrary point in
;;      the current impl.
;;    - [X] agree on what continue/render should dispatch on:
;;      Feedback: it seems ok for now.
;;
;;      Considered done for iteration 26. Will refine it later.
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
                       path'           (conj path '.)
                       edge*           (compile-leaf* edge)
                       [env' edge-sym] (hf-edge->sym! env' edge)]
                   (merge r
                          (if (many? qedge)
                            `{'~qedge
                              @(continue '~path
                                         @(reactive-for (~'fn [~'%]
                                                         (dataflow
                                                          @(continue '~path'
                                                                     (~'let [~edge-sym ~'%]
                                                                      ~(compile-hfql* env& ns-map env' path' cont)))))
                                                        (~'unquote ~(replace* env' edge*))))}
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
   @(dustin.hfql.hfql26/continue
     '[(dustin.fiddle/genders)]
     @(hfdl.lib/reactive-for
       (fn
         [%]
         (hfdl.lang/dataflow
          @(dustin.hfql.hfql26/continue
            '[(dustin.fiddle/genders) .]
            (let
                [% %]
              {':db/ident
               @(dustin.hfql.hfql26/continue
                 '[(dustin.fiddle/genders) . :db/ident]
                 (dustin.hfql.hfql25/hf-nav :db/ident %))}))))
       (unquote (genders))))})

(defmethod continue '[(dustin.fiddle/submissions needle)] [_ v]
  (dataflow
   (into [:table] v) ; because of cardinality N
   ))

(defmethod continue '[(dustin.fiddle/submissions needle) .] [_ v]
  (dataflow [:tr
             (:dustingetz/email v)
             [:td (get-in v [:dustingetz/gender :db/ident])]
             [:td (get-in v [:dustingetz/gender '(dustin.fiddle/shirt-sizes dustingetz/gender)])]]))

(defmethod continue '[(dustin.fiddle/submissions needle) . :dustingetz/email] [_ v]
  (dataflow [:td v]))

(defmethod continue '[(dustin.fiddle/submissions needle) . :dustingetz/gender (dustin.fiddle/shirt-sizes dustingetz/gender)]
  [_ v]
  (dataflow (into [:select]
                  (for [{:keys [db/ident]} v]
                    [:option ident]))))

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
 `{(submissions ~'needle) [:table
                           [:tr
                            [:td "alice@example.com"]
                            [:td :dustingetz/female]
                            [:td
                             [:select
                              [:option :dustingetz/womens-small]
                              [:option :dustingetz/womens-medium]
                              [:option :dustingetz/womens-large]]]]
                           [:tr
                            [:td "bob@example.com"]
                            [:td :dustingetz/male]
                            [:td
                             [:select
                              [:option :dustingetz/mens-small]
                              [:option :dustingetz/mens-medium]
                              [:option :dustingetz/mens-large]]]]
                           [:tr
                            [:td "charlie@example.com"]
                            [:td :dustingetz/male]
                            [:td
                             [:select
                              [:option :dustingetz/mens-small]
                              [:option :dustingetz/mens-medium]
                              [:option :dustingetz/mens-large]]]]],
   (genders)              [#:db{:ident :dustingetz/male}
                           #:db{:ident :dustingetz/female}]}

 (reset! !needle "alice")

 (result program @process)
 :=
 `{(submissions ~'needle) [:table
                           [:tr
                            [:td "alice@example.com"]
                            [:td :dustingetz/female]
                            [:td
                             [:select
                              [:option :dustingetz/womens-small]
                              [:option :dustingetz/womens-medium]
                              [:option :dustingetz/womens-large]]]]]
   (genders)              [#:db{:ident :dustingetz/male}
                           #:db{:ident :dustingetz/female}]})
