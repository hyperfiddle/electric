(ns dustin.hfql23
  (:require
    [minitest :refer [tests]]
    [meander.epsilon :as m]
    [datascript.core :as d]
    [hyperfiddle.api :refer [*$*]]
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]
    [hyperfiddle.hfql20 :refer [many?]]
    [missionary.core :refer [watch latest]]))

(tests
  (many? 'submissions) := true
  (many? 'submission) := false
  )

(comment

  male
  (d/pull *$* [:dustingetz/gender] bob) := {:dustingetz/gender 1}
  (d/entity *$* bob)
  (-> *1 :dustingetz/gender)
  (-> (:dustingetz/gender *1))

  (d/entity *$* male)
  (d/entity *$* :dustingetz/male)
  (d/entity *$* [:db/ident :dustingetz/male])
  (d/entity *$* [:dustingetz/email "alice@example.com"])
  (d/entity *$* 9)

  )

(tests

  (d/pull *$* [:dustingetz/gender] bob) := #:dustingetz{:gender :dustingetz/male}
  (d/pull *$* [:dustingetz/gender :db/id] bob) := {:dustingetz/gender :dustingetz/male, :db/id 10}
  ;(d/pull *$* {:dustingetz/gender [:db/id]} bob) := {:dustingetz/gender }
  (d/pull *$* [{:dustingetz/gender [:db/id]}] bob) := #:dustingetz{:gender #:db{:id :dustingetz/male}}
  ;(d/pull *$* [{:dustingetz/gender [:db/id ]}] bob)
  ;:= #:dustingetz{:gender {:db/id           10,
  ;                         :db/ident        :dustingetz/male,
  ;                         #_#_:dustingetz/type :dustingetz/gender}}
  ;
  ;

  ;(d/pull *$* [:dustingetz/email
  ;             {:dustingetz/gender [:db/id]}
  ;             :dustingetz/shirt-size]
  ;  bob) := #:dustingetz{:gender     #:db{:id :dustingetz/male}
  ;                       :email      "bob@example.com"
  ;                       :shirt-size :dustingetz/mens-medium}

  )

(defn hf-nav [kf ref]
  ; k :db/id or (:db/id %)
  (kf (d/entity *$* ref)))

(tests
  (hf-nav :db/id bob) := 10

  ;(hf-nav identity bob) := 10
  ;(type *1)
  )

(defn resolve-symbol [env s]
  (when-not (contains? env s) (throw (ex-info (str "Could not resolve " s) {:env env})))
  (env s))

(defn hf-apply
  ([edge] (hf-apply edge {}))
  ([edge env] (hf-apply edge env nil))
  ([edge env a]
   (cond
     (keyword? edge) (latest (partial hf-nav edge) a)

     (seq? edge) (let [[f & args] edge
                       f (resolve f)]
                   (->> args
                        (map (fn [arg]
                               (if (symbol? arg)
                                 (resolve-symbol env arg) arg)))
                        (apply latest f)))
     () (do (println "hf-eval unmatched edge: " edge)
            [::unmatched-edge edge]))))

(tests
  (hf-apply :dustingetz/gender {} bob) := :dustingetz/male
  (hf-apply '(identity 10)) := 10
  (hf-apply '(identity foo) {'foo 1}) := 1

  )

(defn form->sym [form]
  (when (keyword? form)
    (symbol (name form))))

(tests
  (form->sym 1) := nil
  (form->sym :dustingetz/female) := 'female
  (form->sym '(identity 1)) := nil
  )

(defn new-ref [id-fn v]
  ^{:react-key (id-fn v)} (atom v))
; we have a new list of react keys and a prior list of react keys
; resulting in a set of additions and removals

(defn hf-pull
  ([pat] (hf-pull pat {}))
  ([pat env] (hf-pull pat env nil))
  ([pat env v]
   (m/match pat
            [!ks ...] (->> !ks
                           (map (fn [k]
                                  (hf-pull k env v)))
                           (apply merge))

            {& (m/seqable !kvs ...)}
            (->> !kvs
                 (map (fn [[form pat]]                      ; [:dustingetz/gender [...]]
                        (let [v (hf-apply form env v)
                              env (assoc env '% v)
                              env (if-some [s (form->sym form)]
                                    (assoc env s v) env)]
                          (if (many? form)
                            ; produce reactive list of reactive elements
                            {form (latest (fn [vs]
                                            (into [] (map (fn [v] (hf-pull pat env (watch (new-ref :db/id v))))) vs)) v)}
                            {form (hf-pull pat env v)}))))
                 (apply merge))

            ?leaf
            {?leaf (hf-apply ?leaf env v)}

            )))

; TODO
; incremental

(tests

  ;(submissions)
  ;(submission)
  ;(genders)
  ;(gender)

  (hf-pull '(submission) {} bob) := {'(submission) 9}
  (hf-pull '(submissions) {} bob) := {'(submissions) [9 10 11]}
  (hf-pull {'(submission) [:db/id]} {} bob) := {'(submission) #:db{:id 9}}
  (hf-pull [{'(submission) [:db/id]}
            {'(gender) [:db/id]}]
           {} bob)

  := '{(submission) #:db{:id 9}, (gender) #:db{:id 1}}

  ;(hf-pull [{'(submissions) [:db/id]}
  ;          '(genders)] bob)

  (hf-pull {'(submissions) [:db/id]} {}) := {'(submissions) [#:db{:id 9} #:db{:id 10} #:db{:id 11}]}

  (hf-pull '[{(submission needle)
              [{:dustingetz/gender
                [{(shirt-size gender)
                  [:db/id :db/ident]}]}]}]
           '{needle "alice"})
  := '{(submission needle) #:dustingetz{:gender {(shirt-size gender) #:db{:id 6, :ident :dustingetz/womens-small}}}}

  (hf-pull '[{(submissions needle)
              [:dustingetz/email
               {:dustingetz/gender
                [{(shirt-sizes gender)
                  [:db/id :db/ident]}]}]}]
           '{needle nil})
  :=
  '{(submissions needle) [#:dustingetz{:email "alice@example.com",
                                       :gender {(shirt-sizes gender) [#:db{:id 6, :ident :dustingetz/womens-small}
                                                                      #:db{:id 7, :ident :dustingetz/womens-medium}
                                                                      #:db{:id 8, :ident :dustingetz/womens-large}]}}
                          #:dustingetz{:email "bob@example.com",
                                       :gender {(shirt-sizes gender) [#:db{:id 3, :ident :dustingetz/mens-small}
                                                                      #:db{:id 4, :ident :dustingetz/mens-medium}
                                                                      #:db{:id 5, :ident :dustingetz/mens-large}]}}
                          #:dustingetz{:email "charlie@example.com",
                                       :gender {(shirt-sizes gender) [#:db{:id 3, :ident :dustingetz/mens-small}
                                                                      #:db{:id 4, :ident :dustingetz/mens-medium}
                                                                      #:db{:id 5, :ident :dustingetz/mens-large}]}}]}

  )

(tests

  (hf-pull [:dustingetz/gender] {} bob) := #:dustingetz{:gender :dustingetz/male}
  (hf-pull [:dustingetz/gender :db/id] {} bob) := {:dustingetz/gender :dustingetz/male :db/id 10}
  ;(hf-pull [{:dustingetz/gender [:dustingetz/type]}] bob) ; datascript issue?
  (hf-pull [{:dustingetz/gender [:db/id]}] {} bob) := {:dustingetz/gender {:db/id 1}}
  (hf-pull [{:dustingetz/gender [:db/ident]}] {} bob) := #:dustingetz{:gender #:db{:ident :dustingetz/male}}
  (hf-pull [{:dustingetz/gender [:db/ident :db/id]}] {} bob) := #:dustingetz{:gender #:db{:ident :dustingetz/male, :id 1}}
  (hf-pull [{:dustingetz/gender [:dustingetz/type]}] {} bob)

  := #:dustingetz{:gender #:dustingetz{:type :dustingetz/gender}}

  (hf-pull [{:dustingetz/gender [:db/id

                                 :db/ident

                                 :dustingetz/type]}] {} bob)

  := #:dustingetz{:gender {:db/id           1,

                           :db/ident        :dustingetz/male,

                           :dustingetz/type :dustingetz/gender}}

  ;(hf-pull [{:dustingetz/gender [(:db/id %)
  ;                               (shirt-sizes gender)
  ;                               :dustingetz/type]}] bob)

  )

(tests

  "meander"

  (m/match {:a 1 :b 2 :c 3}                                 ;
           ;{& (m/seqable ?x)} ?x
           ;{& (m/seqable [?edge ?pat])} [?pat]
           ;{& (m/seqable [!x ...])} [!x]
           {& (m/seqable !entries ...)} !entries
           ;{& ?x} ?x
           ;{& (m/seqable [!edge !pat] ...)} [!edge !pat]
           ?else :else)

  := [[:a 1] [:b 2] [:c 3]]
  )

;(hf-pull :dustingetz/gender bob) := 1
;(hf-pull [:dustingetz/gender] bob) := [1]
;(hf-pull [:dustingetz/gender :db/id] bob) := [1]
;(hf-pull {:dustingetz/gender [:db/id]} bob) :=
;(hf-pull [{:dustingetz/gender [:db/id]}] bob) :=

(tests
  (def target (atom bob))
  (def res (hf-pull [:dustingetz/gender] {} (watch target)))
  (def it ((:dustingetz/gender res) #(prn :ready) #(prn :done)))
  @it := :dustingetz/male
  (reset! target bob)
  @it := :dustingetz/female


  (def address (atom "bob"))
  (def res (hf-pull '[{(submission needle)
                       [{:dustingetz/gender
                         [{(shirt-size gender)
                           [:db/id :db/ident]}]}]}]
                    {'needle (watch address)}))

  (def flow (get-in res '[(submission needle) :dustingetz/gender (shirt-size gender) :db/ident]))
  (def it (flow #(prn :ready) #(prn :done)))

  (reset! address "")
  @it



  (def res (hf-pull '[{(submissions needle)
                       [{:dustingetz/gender
                         [{(shirt-sizes gender)
                           [:db/id :db/ident]}]}]}]
                    {'needle (watch address)}))
  res
  (def flow (get res '(submissions needle)))
  (def it (flow #(prn :ready) #(prn :done)))
  (def current @it)
  current
  (def nested (get-in current '[0 :dustingetz/gender (shirt-sizes gender)]))
  nested
  (def it2 (nested #(prn :ready) #(prn :done)))
  (def nested-nested @it2)
  nested-nested
  (def it3 ((get-in nested-nested [0 :db/ident]) #(prn :ready) #(prn :done)))
  @it3

  )