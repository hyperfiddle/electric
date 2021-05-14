;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "A unification library for Clojure."
      :author "Michael Fogus"}
  hyperfiddle.rcf.unify
  (:require [clojure.walk :as walk]
            #?(:clj [hyperfiddle.rcf.unify-macro :refer [create-var-unification-fn]])
            #?(:cljs [hyperfiddle.rcf.unify-macro :refer-macros [create-var-unification-fn]])))

(defn ignore-variable? [sym] (= '_ sym))

;; ### Utilities

(def lvar? #(or (ignore-variable? %)
                (and (symbol? %) (re-matches #"^\?.*" (name %)))))

(defn extract-lvars
  "Takes a datastructure and returns a distinct set of the logical
   variables found within."
  ([form]
     (extract-lvars lvar? form))
  ([lv-fn form]
     (set
      (walk/walk #(when (lv-fn %) %)
                 #(keep identity %)
                 form))))

(defn- composite?
  "Taken from the old `contrib.core/seqable?`. Since the meaning of 'seqable' is
   questionable, I will work on phasing it out and using a more meaningful
   predicate.  At the moment, the only meaning of `composite?` is:
   Returns true if `(seq x)` will succeed, false otherwise."
  [x]
  (seqable? x)
  #_(or (coll? x)
      (nil? x)
      (instance? Iterable x)
      (-> x class .isArray)
      (string? x)
      (instance? java.util.Map x)))

(declare garner-unifiers)

(def ^{:doc "Unify the variable v with expr.  Uses the bindings supplied and possibly returns an extended bindings map."
       :private true}
  unify-variable (create-var-unification-fn true))

(def ^{:doc "Unify the variable v with expr.  Uses the bindings supplied and possibly returns an extended bindings map."
       :private true}
  unify-variable- (create-var-unification-fn false))

(defn wildcard? [form]
  (and (composite? form)
       (#{'&} (first form))))

(defn- garner-unifiers
  "Attempt to unify x and y with the given bindings (if any). Potentially returns a map of the
   unifiers (bindings) found.  Will throw an `IllegalStateException` if the expressions
   contain a cycle relationship.  Will also throw an `IllegalArgumentException` if the
   sub-expressions clash."
  ([x y]                 (garner-unifiers unify-variable lvar? x y {}))
  ([variable? x y]       (garner-unifiers unify-variable variable? x y {}))
  ([variable? x y binds] (garner-unifiers unify-variable variable? x y binds))
  ([uv-fn variable? x y binds]
     (cond
      (not binds)               nil
      (= x y)                   binds
      (variable? x)             (uv-fn variable? x y binds)
      (variable? y)             (uv-fn variable? y x binds)
      (wildcard? x)             (uv-fn variable? (second x) (seq y) binds)
      (wildcard? y)             (uv-fn variable? (second y) (seq x) binds)
      (every? composite? [x y]) (garner-unifiers uv-fn
                                                 variable?
                                                 (rest x)
                                                 (rest y)
                                                 (garner-unifiers uv-fn
                                                                  variable?
                                                                  (first x)
                                                                  (first y)
                                                                  binds)))))

(defn flatten-bindings
  "Flattens recursive bindings in the given map to the same ground (if possible)."
  ([binds] (flatten-bindings lvar? binds))
  ([variable? binds]
     (into {}
           (remove (comp nil? second)
                   (map (fn [[k v]]
                          [k (loop [v v]
                               (if (variable? v)
                                 (recur (binds v))
                                 v))])
                        binds)))))

(defn- try-subst
  "Attempts to substitute the bindings in the appropriate locations in the given expression."
  [variable? x binds]
  {:pre [(map? binds) (fn? variable?)]}
  (walk/prewalk (fn [expr]
                  (if (variable? expr)
                    (or (binds expr) expr)
                    expr))
                x))

(defn- unifier*
  "Attempts the entire unification process from garnering the bindings to substituting
   the appropriate bindings."
  ([x y] (unifier* lvar? x y))
  ([variable? x y]
     (unifier* variable? x y (garner-unifiers variable? x y)))
  ([variable? x y binds]
     (->> binds
          (flatten-bindings variable?)
          (try-subst variable? y))))   ;; y is arbitrary

;; #  PUBLIC API

;; ## OCCURS

(defn make-occurs-unify-fn
  "Given a function to recognize unification variables, returns a function to
   return a bindings map for two expressions.  This function uses an 'occurs check'
   methodology for detecting cycles."
  [variable-fn]
  (fn
    ([x y] (garner-unifiers unify-variable variable-fn x y {}))
    ([x y binds] (garner-unifiers unify-variable variable-fn x y binds))))

(defn make-occurs-subst-fn
  "Given a function to recognize unification variables, returns a function that
   will attempt to substitute unification bindings between two expressions.
   This function uses an 'occurs check' methodology for detecting cycles."
  [variable-fn]
  (partial try-subst variable-fn))

(defn make-occurs-unifier-fn
  "Given a function to recognize unification variables, returns a function to
   perform the unification of two expressions. This function uses an 'occurs check'
   methodology for detecting cycles."
  [variable-fn]
  (partial unifier* variable-fn))


(def ^{:arglists '([expression1 expression2])}
  unify   (make-occurs-unify-fn lvar?))

(def ^{:doc      "" ;; #_(:doc (meta #'try-subst))
       :arglists '([expression bindings])}
  subst   (make-occurs-subst-fn lvar?))

(def ^{:arglists '([expression1 expression2])}
  unifier (make-occurs-unifier-fn lvar?))

;; ## NO OCCURS

(defn make-unify-fn
  "Given a function to recognize unification variables, returns a function to
   return a bindings map for two expressions."
  [variable-fn]
  (fn
    ([x y] (garner-unifiers unify-variable- variable-fn x y {}))
    ([x y binds] (garner-unifiers unify-variable- variable-fn x y binds))))

(defn make-subst-fn
  "Given a function to recognize unification variables, returns a function that
   will attempt to substitute unification bindings between two expressions."
  [variable-fn]
  (partial try-subst variable-fn))

(defn make-unifier-fn
  "Given a function to recognize unification variables, returns a function to
   perform the unification of two expressions."
  [variable-fn]
  (fn [x y]
    (unifier* variable-fn
              x
              y
              (garner-unifiers unify-variable- variable-fn x y {}))))


(def ^{:arglists '([expression1 expression2])}
  unify-   (make-unify-fn lvar?))


(def ^{:arglists '([expression1 expression2])}
  unifier- (make-unifier-fn lvar?))


(comment

  (def T '[?a ?b])

  (defn make-matcher
    [root]
    (fn [tmpl req]
      (-> req
          :url
          (.replace root "")
          (.split "/")
          seq
          (unify tmpl))))

  (def match (make-matcher "http://foo.com/foo/"))

  (match T {:url "http://foo.com/foo/fee/fi"})
  ;;=> {?a "fee", ?b "fi"}

  (match T {:url "http://foo.com/foo/flub"})
  ;;=> {?a "flub"}

  (defn make-genr
    [root]
    (fn [tmpl binds]
      (->> binds
           (subst tmpl)
           (interpose "/")
           (cons root)
           (apply str))))

  (def gen (make-genr "http://foo.com/foo/"))

  (gen T '{?a 1, ?b 2})
  ;;=> "http://foo.com/foo/1/2"

  (unify {:first '?first
          :last  '?last
          :genre :giallo}

         {:first "Dario"
          :last  "Argento"
          :genre :giallo})

  ;;=> {?first "Dario", ?last "Argento"}

  (unifier ['?first "Argento"]
           ["Dario" '?last])

  ;;=> ["Dario" "Argento"]


  (subst '[1 2 ?x ?y]
         '{?x [3 4 ?y 6]})

  ;;=> [1 2 [3 4 ?y 6] ?y]


  (unifier '[(?a * ?x | 2) + (?b * ?x) + ?c]
           '[?z + (4 * 5) + 3])

  ;;=> [(?a * 5 | 2) + (4 * 5) + 3]

  (unify '[(?a * ?x | 2) + (?b * ?x) + ?c]
         '[?z + (4 * 5) + 3])

  ;;=> {?c 3, ?x 5, ?b 4, ?z (?a * ?x | 2)}


  (unify '[(?a * ?x | 2) + (?b * ?x) + ?c]
         '[(?a * 5 | 2) + (4 * 5) + 3])

  ;;=> {?c 3, ?b 4, ?x 5}

  (unify '[(?a * 5 | 2) + (4 * 5) + 3]
         '[?z + (4 * 5) + 3])

  ;;=> {?z (?a * 5 | 2)}

  (= (subst '[?z + (4 * 5) + 3]
            '{?c 3, ?b 4, ?x 5
              ?z (?a * 5 | 2)})

     (subst '[(?a * ?x | 2) + (?b * ?x) + ?c]
            '{?c 3, ?b 4, ?x 5
              ?z (?a * 5 | 2)}))

  ;;=> true

  (subst '[(?a * ?x | 2) + (?b * ?x) + ?c]
         '{?c 3, ?b 4, ?x 5})

  ;;=> [(?a * 5 | 2) + (4 * 5) + 3]

  (unify [1 2 3] '[?x & ?more])
  ;;=> {?more (2 3), ?x 1}

  (unify [1 2 3] '[_ _ _ & ?more])
  ;;=> {}

  (unify [1 2 3 4 5] '[_ _ _ & ?more])
  ;;=> {?more (4 5)}

  (unify [1 2 3 4 5] '[_ ?b _ & ?more])
  ;;=> {?more (4 5), ?b 2}

  (unify [:foo 1 2] '[?head & _])

  ;;=> {?head :foo}

  (unifier [1 2 3] '[?x & ?more])

)
