(ns geoffrey.hfql2
  (:require [clojure.test :refer [is]]
            [clojure.walk :as walk]
            [hyperfiddle.fabric :as df]
            [hyperfiddle.hfql-examples3 :as examples])
  (:import hyperfiddle.View))
​
(def a '[{(submissions needle) [{:gender [:db/ident
                                          {(shirt-sizes gender) [*]}]}]}
         {(genders) [*]}])
​
(assert (is (= examples/a a)))
​
;; --------------------------------------------------------------------------------
​
;; in the context of HFQL AST all literal lists are sexps. The `list?` predicate
;; is not enough it returns false for Cons cells which we use later on.
​
(def sexp? seq?)
​
(defn remove-sexps [x]
  (walk/postwalk (fn [x]
                   (cond
                     (sexp? x)      nil
                     (map? x)       (let [m (dissoc x nil)]
                                      (if (empty? m)
                                        nil
                                        m))
                     (map-entry? x) x
                     (vector? x)    (filterv some? x)
                     :else          x))
    x))
​
(def b (let [env    (atom #{})
             points (atom {})
             parent (atom nil)
             ask    (fn [x]
                      (if (ident? x)
                        (let [points @points]
                          (or (get points x)
                            (get points (name x))
                            (get points (keyword x))
                            x))
                        x))]
         (walk/prewalk (fn [x]
                         (cond
                           (and (map-entry? x)
                             (sexp? (key x)))    (let [pp         (remove-sexps (val x))
                                                       [f & args] (key x)
                                                       form       (->> args
                                                                    (map ask)
                                                                    (cons (list 'quote pp))
                                                                    (cons f))]
                                                   (reset! parent form)
                                                   (swap! env conj form)
                                                   x)
                           (and (map-entry? x)
                             (keyword? (key x))) (let [form (list (key x) @parent)]
                                                   (swap! env conj form)
                                                   (swap! points assoc (key x) form)
                                                   x)
                           :else                    x))
           a)
         @env)
  )
​
(assert (is (= examples/b b)))
​
;; --------------------------------------------------------------------------------
​
(defn inject-$
  [env x]
  (cond
    (contains? env x) (let [[f & args] x]
                        (if (keyword? f)
                          (cons f (map (partial inject-$ env) args))
                          (cons f (cons '$ (map (partial inject-$ env) args)))))
    (sexp? x)         (map (partial inject-$ env) x)
    :else             x))
​
(def c (let [env b]
         (-> (map (partial inject-$ env) env)
           (set))))
​
(assert (is (= examples/c c)))
​
;; --------------------------------------------------------------------------------
​
(defn inject-reader-ask
  [env x]
  (cond
    (or (contains? env x)
      (symbol? x)) (list 'ask (list 'quote x))
    (sexp? x)        (let [[f & args] x]
                       (cons f (map (partial inject-reader-ask env) args)))
    :else            x))
​
(def d (let [env c]
         (->> env
           (map (fn [form]
                  [form (inject-reader-ask (if (sexp? form)
                                             (disj env form)
                                             env)
                          form)]))
           (into {}))))
​
(assert (is (= examples/d d)))
​
;; --------------------------------------------------------------------------------
​
(defn ask? [x] (and (sexp? x) (= 'ask (first x))))
​
(defn ask->fmap [form]
  (if (keyword? (first form))
    (cons 'fmap form)
    (let [asks (->> (filter ask? form)
                 (map (fn [form] [form (gensym)])))]
      `(~'fmap (~'fn [~@(map second asks)] ~(replace (into {} asks) form)) ~@(map first asks)))))
​
(with-redefs [gensym (let [syms (atom (map #(symbol (str "sym" %)) (range)))]
                       (fn []
                         (let [[s & rest] @syms]
                           (reset! syms rest)
                           s)))]
  (def e (->> d
           (map (fn [[k v]]
                  [k (ask->fmap v)]))
           (into {}))))
​
(assert (is (= examples/e e)))
​
​
;; --------------------------------------------------------------------------------
​
(defn genders [$ pp]
  (prn "time basis is " $ (str (range 1 10)))
  [:male :female])
​
(defn shirt-sizes [$ pp gender]
  (case gender
    :male   [:mens-small :mens-medium :mens-large]
    :female [:woman-small :woman-medium :woman-large]))
​
(defn submissions [$ pp needle]
  (prn needle)
  {:id     1
   :gender :male})
​
​
;; --------------------------------------------------------------------------------
​
(def ^:dynamic *env* {})
​
(def reified? (complement sexp?))
​
(defn ask
  ([x]
   (ask *env* x))
  ([env x]
   (let [v (get @env x ::not-found)]
     (if (= ::not-found v)
       (when (symbol? x)
         (eval x))
       (if (reified? v)
         v
         (do (swap! env update x eval)
             (ask env x)))))))
​
(def ^:dynamic *context* :identity)
​
(defn fmap [f & vs]
  (case *context*
    :identity (apply f vs)
    :streams  (apply df/fmap f vs)))
​
(defn eval-env [env]
  (reduce-kv (fn [env k v]
               (assoc env k (eval v)))
    env
    env))
​
​
'{(submissions needle)                         {:id     1
                                                :name   "bob"
                                                :gender :male}
  (:gender (submissions needle))               :male
  (shirt-sizes (:gender (submissions needle))) [:small :large :medium]
  }
​
'{(submissions "John")                         View
  (:gender (submissions "John"))               View
  (shirt-sizes (:gender (submissions "John"))) View}
​
(comment
  ;; -------- Dataflow --------

  (let [$      (df/input)
        needle (df/input)
        env    (assoc e '$ $, 'needle needle)]              ; e
    (binding [*context* :streams
              *env*     (atom env)]
      (->> (doto (eval-env env) prn)
        (run! (fn [[k >v]]
                (when (instance? View >v)
                  (df/on >v (fn [v]
                              (println k "  =>  " v)))))))
      (do
        (println "Setting time basis")
        (df/put $ {:time-basis 0})
        (println "\n--------\n")
        (println "Setting a needle")
        (df/put needle {:new :map})
        (println "\n--------\n")
        (println "Changing a needle")
        (df/put needle {:new2 :map})
        (df/put needle {:new2 :map})
        )))
  ​
  ;; --------    V2   ---------
  ​
  (let [$      (df/input)
        needle (df/input)
        env    (assoc e '$ $ 'needle needle)                ; e
        outer  (df/input)]
    (binding [*context* :streams
              *env*     (atom env)]
      (-> (df/fmap eval-env outer)
        (df/on (fn [m] (clojure.pprint/pprint m)
                 (run! (fn [[k >v]]
                         (df/on >v (fn [v] (println [k v]))))
                   m))))
      (do
        (println "Evaluting the env")
        (df/put outer env)
        (println "\n--------\n")
        (println "Setting time basis")
        (df/put $ {:time-basis 0})
        (println "\n--------\n")
        (println "Setting a needle")
        (df/put needle :male)
        (println "\n--------\n")
        (println "Changing a needle")
        (df/put needle :female)
        (df/put needle :female))))
  ​
  ;; -------- Identity --------
  (def needle :female)
  ​
  (clojure.pprint/pprint
    (let [env (assoc e '$ {:time-basis 0})]
      (binding [*context* :identity
                *env*     (atom env)]
        (eval-env env))))
  )
