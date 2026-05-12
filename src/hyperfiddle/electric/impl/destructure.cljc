(ns hyperfiddle.electric.impl.destructure
  "Cross-platform (clj/cljs) destructuring. Adapted from clojurescript codebase"
  (:require [cljs.core]
            [hyperfiddle.electric.impl.lang3 :as-alias lang]
            [hyperfiddle.electric.impl.runtime3 :as r]))

(defn- pattern-site [pat]
  (let [m (meta pat)
        s? (:hyperfiddle.electric3/server m)
        c? (:hyperfiddle.electric3/client m)]
    (cond (and s? c?) (throw #?(:clj (new Exception
                                       (str "Destructure pattern carries both ^::e/server and ^::e/client metadata: " pat))
                                :cljs (new js/Error
                                       (str "Destructure pattern carries both ^::e/server and ^::e/client metadata: " pat))))
          s? :server
          c? :client
          :else nil)))

(defn destructure* [bindings]
  (let [bents (partition 2 bindings)
        pb (fn pb [bvec b v site]
             (let [wrap (fn [form] (if site (list ::lang/site site form) form))
                   pvec
                   (fn [bvec b val]
                     (let [gvec (gensym "vec__")
                           gseq (gensym "seq__")
                           gfirst (gensym "first__")
                           has-rest (some #{'&} b)]
                       (loop [ret (let [ret (conj bvec gvec val)]
                                    (if has-rest
                                      (conj ret gseq (wrap (list `seq gvec)))
                                      ret))
                              n 0
                              bs b
                              seen-rest? false]
                         (if (seq bs)
                           (let [firstb (first bs)]
                             (cond
                               (= firstb '&) (recur (pb ret (second bs) gseq site)
                                               n
                                               (nnext bs)
                                               true)
                               (= firstb :as) (pb ret (second bs) gvec site)
                               :else (if seen-rest?
                                       (throw #?(:clj (new Exception "Unsupported binding form, only :as can follow & parameter")
                                                 :cljs (new js/Error "Unsupported binding form, only :as can follow & parameter")))
                                       (recur (pb (if has-rest
                                                    (conj ret
                                                      gfirst (wrap `(first ~gseq))
                                                      gseq (wrap `(next ~gseq)))
                                                    ret)
                                                firstb
                                                (if has-rest
                                                  gfirst
                                                  (wrap (list `nth gvec n nil)))
                                                site)
                                         (inc n)
                                         (next bs)
                                         seen-rest?))))
                           ret))))
                   pmap
                   (fn [bvec b v]
                     (let [gmap (gensym "map__")
                           defaults (:or b)]
                       (loop [ret (-> bvec (conj gmap) (conj v)
                                    (conj gmap) (conj (wrap `(r/get-destructure-map ~gmap)))
                                    ((fn [ret]
                                       (if (:as b)
                                         (conj ret (:as b) gmap)
                                         ret))))
                              bes (let [transforms
                                        (reduce
                                          (fn [transforms mk]
                                            (if (keyword? mk)
                                              (let [mkns (namespace mk)
                                                    mkn (name mk)]
                                                (cond (= mkn "keys") (assoc transforms mk #(keyword (or mkns (namespace %)) (name %)))
                                                      (= mkn "syms") (assoc transforms mk #(list `quote (symbol (or mkns (namespace %)) (name %))))
                                                      (= mkn "strs") (assoc transforms mk str)
                                                      :else transforms))
                                              transforms))
                                          {}
                                          (keys b))]
                                    (reduce
                                      (fn [bes entry]
                                        (reduce #(assoc %1 %2 ((val entry) %2))
                                          (dissoc bes (key entry))
                                          ((key entry) bes)))
                                      (dissoc b :as :or)
                                      transforms))]
                         (if (seq bes)
                           (let [bb (key (first bes))
                                 bk (val (first bes))
                                 local (if #?(:clj  (instance? clojure.lang.Named bb)
                                              :cljs (cljs.core/implements? INamed bb))
                                         (with-meta (symbol nil (name bb)) (meta bb))
                                         bb)
                                 bv (if (contains? defaults local)
                                      (wrap (list 'get gmap bk (defaults local)))
                                      (wrap (list 'get gmap bk)))]
                             (recur
                               (if (or (keyword? bb) (symbol? bb)) ;(ident? bb)
                                 (-> ret (conj local bv))
                                 (pb ret bb bv site))
                               (next bes)))
                           ret))))]
               (cond
                 (symbol? b) (-> bvec (conj (if (namespace b) (symbol (name b)) b)) (conj v))
                 (keyword? b) (-> bvec (conj (symbol (name b))) (conj v))
                 (vector? b) (pvec bvec b v)
                 (map? b) (pmap bvec b v)
                 :else (throw
                         #?(:clj (new Exception (str "Unsupported binding form: " b))
                            :cljs (new js/Error (str "Unsupported binding form: " b)))))))
        process-entry (fn [bvec b]
                        (let [pat (first b)]
                          (pb bvec pat (second b) (pattern-site pat))))]
    (if (every? symbol? (map first bents))
      bindings
      (if-let [kwbs (seq (filter #(keyword? (first %)) bents))]
        (throw
          #?(:clj (new Exception (str "Unsupported binding key: " (ffirst kwbs)))
             :cljs (new js/Error (str "Unsupported binding key: " (ffirst kwbs)))))
        (reduce process-entry [] bents)))))
