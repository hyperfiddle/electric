(ns hyperfiddle.q7
  (:refer-clojure :exclude [keyword? name namespace])
  (:import (clojure.lang Compiler$LocalBinding IObj))
  (:require [cljs.analyzer.api :as ana-api]
            [clojure.string :as str]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.rcf :as rcf :refer [tests ! %]]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.walk :as walk]
            [user.gender-shirt-size :refer [submissions submission shirt-sizes genders emails sub-profile]]
            [clojure.spec.alpha :as s]))

(defrecord Kw [form]) ;; a wrapper to add meta on keywords

(defn get-form [x] (:form x x))

(defn keyword? [k]
  (or (instance? Kw k)
      (clojure.core/keyword? k)))

(defn name [named]
  (if (instance? Kw named)
    (recur (get-form named))
    (clojure.core/name named)))

(defn namespace [named]
  (if (instance? Kw named)
    (recur (get-form named))
    (clojure.core/namespace named)))

(defn quoted? [form]
  (and (seq? form)
       (= 'quote (first form))))

(defn quoting [form]
  (cond
    (symbol? form) (list 'quote form)
    (quoted? form) form
    (seq? form)    (list 'quote form)
    :else          form))

(defn symbolic
  ([form]
   (symbolic identity form))
  ([formatf form]
   (cond
     (keyword? form)  (formatf (get-form form))
     (map? form)      (recur formatf (ffirst form))
     (or (symbol? form)
         (seq? form)) (formatf form)
     :else            (throw (ex-info "Not a named field" {:form form})))))

(defn edge-name [form]
  (symbolic (fn [form] (symbol (str/join "_" (remove nil? [(namespace form) (name form)]))))
                 form))

(declare emit)

(defn emit-props [form]
  (walk/prewalk get-form (::props (meta form))))

(defn emit-nav [form] `(binding [hf/props ~(emit-props form)
                                 hf/value (hf/nav hf/entity ~(get-form form))] (unquote render)))

(defn emit-call [form]
  (let [form (get-form form)]
    (if (= `p/$ (first form))
      form
      (cons `p/$ form))))

(defn emit-attribute [form]
  (let [sym (symbolic form)]
    (if (or (symbol? sym)
            (seq? sym))
      (list 'quote sym)
      sym)))

(defn cardinality [attr]
  (let [attr (symbolic attr)]
    (cond
      (seq? attr) (recur (first (remove #{`p/$} attr)))
      :else       (spec/cardinality attr))))

;; TODO dynamic multiplicity (query db, not only spec)
(defn emit-traversal [form]
  (let [[k v]     (first form)
        attribute (emit-attribute k)
        card      (cardinality k)]
    (cond
      (empty? v)           `(binding [hf/attribute ~attribute
                                      hf/props     ~(emit-props v)]
                              ~(emit k))
      (= ::spec/one card)  `(binding [hf/columns   ~(mapv symbolic v)
                                      hf/attribute ~attribute
                                      hf/entity    ~(emit k)]
                              ~(emit v))
      (= ::spec/many card) `(binding [hf/columns   ~(mapv symbolic v)
                                      hf/attribute ~attribute
                                      hf/props     ~(emit-props v)
                                      hf/value     #'(p/for [e# ~(emit k)]
                                                       #'(binding [hf/entity e#]
                                                           ~(emit v)))]
                              (unquote render))
      :else                (throw (ex-info "Can’t infer cardinality." {:form k})))))

(defn edge->traversal [form] (if (and (map? form) (not (record? form))) form {form []}))

(defn emit-edges [form]
  (let [props (emit-props form)
        form  (map edge->traversal form)
        forms (zipmap (map emit-attribute form) (map (fn [form] `#'~(emit form)) form))]
    `(binding [hf/props   ~props
               hf/columns ~(mapv emit-attribute form)
               hf/value   #'~forms]
       (unquote render))))

(defn emit [form]
  (cond
    (keyword? form) (emit-nav form)
    (vector? form)  (emit-edges form)
    (map? form)     (emit-traversal form)
    (seq? form) (emit-call form)
    :else           (throw (ex-info "unknow form" {:form form}))))

(defn props? [form]
  (and (seq? form)
       (= '. (second form))
       (even? (count (drop 2 form)))
       (every? keyword? (map first (partition 2 (drop 2 form))))))

(defn parse-props [form]
  (let [[_form _dot & props] form]
    (->> (partition 2 props)
         (map (fn [[k v]] [(get-form k) v]))
         (into {}))))

(defn parse [form]
  (walk/postwalk (fn [form]
                   (cond
                     (map? form)     (let [[k v] (first form)]
                                       {(vary-meta k dissoc ::props)
                                        (vary-meta v assoc ::props (::props (meta k)))})
                     (keyword? form) (->Kw form)
                     (props? form)   (vary-meta (first form) assoc ::props (parse-props form))
                     :else           form))
                form))

(defn meta? [form] #?(:clj (instance? IObj form) :cljs (satisfies? IMeta form)))

(defn update* [m k f & args]
  (if (contains? m k)
    (apply update m k f args)
    m))

(defn reactive-quote? [form] (and (seq? form) (= 'var (first form))))

(defn continuation-on-options [form]
  (walk/postwalk (fn [form]
                   (if (and (map? form) (not (record? form)))
                     (let [[k v] (first form)]
                       {k (vary-meta v update* ::props update* ::hf/options
                                     (fn [options]
                                       (let [v    (vary-meta v dissoc ::props)
                                             body (emit (cond
                                                          (reactive-quote? options) {(second options) v}
                                                          (symbol? options)         `{(~options) ~v}))]
                                         `#'(binding [render-mode ::edn] ~body))))})
                     form))
                 form))

;;;;;;;;;
;; ENV ;;
;;;;;;;;;

(defn resolve-ns-alias [env sym]
  (let [ns  (namespace sym)
        nom (name sym)]
    (when-let [qualified (get (::aliases env) ns)]
      (symbol qualified nom))))

(defn resolve' [env sym]
  (assert (symbol? sym) (str "Can’t resolve " (pr-str sym)))
  (if (:js-globals env)
    (:name (ana-api/resolve env sym))
    (or (resolve-ns-alias env sym)
        (let [?var (get (::globals env) sym)]
          (if (var? ?var)
            (symbol ?var)
            sym)))))

(defn make-env [&env]
  (let [env (into {} (reduce-kv (fn [r k v] (if (instance? clojure.lang.Compiler$LocalBinding v)
                                              (assoc r (.-sym v) (.-sym v))
                                              (assoc r k v))) {} &env))]
    (-> (assoc env ::globals (ns-map *ns*))
        (assoc ::aliases (reduce-kv (fn [r k v] (assoc r (str k) (str v))) {} (ns-aliases *ns*)))
        (vary-meta assoc :type ::env))))

(defn resolve-syms [env form]
  (walk/prewalk (fn [form]
                  (cond
                    (quoted? form) (reduced form)
                    (symbol? form) (resolve' env form)
                    :else          form))
                form))

;;;;;;;;;;;;;;;;
;; ENTRYPOINT ;;
;;;;;;;;;;;;;;;;

(defmacro hfql [form]
  (let [form (if (map? form) [form] form)]
    (->> form
         (resolve-syms (make-env &env))
         (parse)
         (continuation-on-options)
         (emit))))

(p/def render-mode ::user) ;; #{::edn ::default ::user}

(p/def render #'(case render-mode
                  ::default ~hf/render
                  ::edn     ~hf/sequenceM
                  ::user    (if-let [renderer (::hf/render hf/props)]
                              (do (assert (fn? renderer) "::hf/render must be a reactive function")
                                  ~renderer)
                              ~hf/render)))

;;;;;;;;;;;
;; TESTS ;;
;;;;;;;;;;;

(do
  (tests
   (props? 'a)           := false
   (props? '(a . b))     := false
   (props? '(a . ::b))   := false
   (props? '(a . ::b 1)) := true)

  (tests
   (edge-name 'a)    := 'a
   (edge-name 'ns/a) := 'ns_a
   (edge-name :a)    := 'a
   (edge-name :ns/a) := 'ns_a)

  (tests
   (p/run (! (binding [hf/entity 9] (hfql :db/id) )))
   % := 9)

  (tests
   (p/run (! (binding [hf/entity 9] (hfql [:db/id]) )))
   % := {:db/id 9})

  (p/def string-renderer #'(str ~hf/value))

  (tests
   "hf/render"
   (p/run (! (binding [hf/entity 9] (hfql (:db/id . ::hf/render string-renderer)) )))
   % := "9")

  (tests
   "hf/render inline"
   (p/run (! (binding [hf/entity 9] (hfql (:db/id . ::hf/render #'(str ~hf/value))) )))
   % := "9")

  (tests
   (p/run (! (binding [hf/entity 9] (hfql [(:db/id . ::hf/render string-renderer)]) )))
   % := {:db/id "9"})

  (tests
   (p/run (binding [hf/entity 9] (! (hfql {:dustingetz/gender []}) )))
   % := {:dustingetz/gender 2})

  (tests
   (p/run (binding [hf/entity 9] (! (hfql [{:dustingetz/gender []}]) )))
   % := {:dustingetz/gender 2})


  (tests
   (p/run (binding [hf/entity 9] (! (hfql {:dustingetz/gender [:db/ident]}) )))
   % := {:dustingetz/gender {:db/ident :dustingetz/female}})

  (tests
   (p/run (binding [hf/entity 9] (! (hfql [{:dustingetz/gender [:db/ident]}]) )))
   % := {:dustingetz/gender {:db/ident :dustingetz/female}})

  (tests
   (p/run (! (hfql {(submission "") [:db/id]}) ))
   % := {'(user.gender-shirt-size/submission "") {:db/id 9}})

  (tests
   "EAV"
   (p/run (! (hfql {(submission "") [(:dustingetz/email . ::hf/render #'[hf/entity hf/attribute ~hf/value])]}) ))
   % := '{(user.gender-shirt-size/submission "") {:dustingetz/email [9 :dustingetz/email "alice@example.com"]}})

  (tests
   (p/run (! (hfql {(submission "") [{:dustingetz/shirt-size [:db/ident]}]}) ))
   % := {'(user.gender-shirt-size/submission "") #:dustingetz{:shirt-size #:db{:ident :dustingetz/womens-large}}})

  (tests
   "multiplicity many"
   (p/run (! (hfql {(submissions "") [:db/id]}) ))
   % := {'(user.gender-shirt-size/submissions "") [{:db/id 9} {:db/id 10} {:db/id 11}]})

  (tests
   (p/run (! (hfql {(submissions "") [(:db/id . ::hf/render string-renderer)]}) ))
   % := {'(user.gender-shirt-size/submissions "") [{:db/id "9"} {:db/id "10"} {:db/id "11"}]})


  (do
    (defn fail []
      (throw (ex-info "I fail" {})))

    (p/def throwing-renderer #'(fail))

    (p/def ignoring-renderer #'"ignored")

    (tests
     (p/run (! (binding [hf/entity 9]
                 (hfql [{(:dustingetz/gender . ::hf/render ignoring-renderer) [(:db/ident . ::hf/render throwing-renderer)]}]) )))
     % := #:dustingetz{:gender "ignored"}
     ;; note it didn’t throw
     ))

  (do

    (p/defn join-all []
      (binding [hf/render hf/sequenceM]
        ~hf/render))

    (p/def select-option-renderer
      #'(into [:select {:value ~join-all}]
              (let [options (::hf/options hf/props)]
                ;; (prn "options" options)
                 ;; (prn "~options" ~options)
                (p/for [e ~options]
                  [:option e]))))

    (tests
     (p/run (! (binding [hf/entity 9]
                 (hfql (:dustingetz/shirt-size . ::hf/render select-option-renderer
                                               ::hf/options #'(p/$ shirt-sizes :dustingetz/female "")
                                               )) 
                 )))
     %
     := [:select {:value 8} [:option 6] [:option 7] [:option 8]]
     )
    )

  (tests
   (p/run (! (binding [hf/entity 9]
               (hfql {(:dustingetz/shirt-size . ::hf/render select-option-renderer
                                              ::hf/options #'(p/$ shirt-sizes :dustingetz/female "")
                                              )
                      [:db/ident]}) )))
   % := {:dustingetz/shirt-size [:select {:value #:db{:ident :dustingetz/womens-large}}
                                 [:option #:db{:ident :dustingetz/womens-small}]
                                 [:option #:db{:ident :dustingetz/womens-medium}]
                                 [:option #:db{:ident :dustingetz/womens-large}]]})

  )


(comment



  )
