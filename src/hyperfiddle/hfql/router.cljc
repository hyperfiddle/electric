(ns hyperfiddle.hfql.router
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-xp :as xp]
   [hyperfiddle.spec :as spec]
   #?(:clj [hyperfiddle.q9 :as hfql])
   #?(:clj [hyperfiddle.q9.env :as env]))
  #?(:cljs (:require-macros [hyperfiddle.hfql.router :refer [not-found]]
                            [hyperfiddle.photon-xp :as xp])))

(defn- fncall [sexpr] (and (seq? sexpr) (symbol? (first sexpr))))

(defn validate-route! [route]
  (when (not (fncall route))
    (throw (ex-info (str "Invalid route. Routes are symbolic function calls like `(foo :bar)`, given `" route "`.") {:route route}))))

(defn validate-pages! [pages]
  (assert (seq pages) "A router requires at least one page to route to.")
  (doseq [page pages]
    (assert (map? page) (str "A router can only route to a page. A page is a single key-value HFQL map. Given `" (pr-str page) "`"))
    (let [keys (keys page)]
      (assert (= 1 (count keys)) (str "A routable page must have a single entrypoint (root). The given page declares " (count keys) " entrypoints: " (pr-str keys) ". Please choose one. In `" (pr-str page) "`.")))))

(defn- identifier [call]
  (let [[f & args] call]
    (case f
      props (identifier (first args))
      f)))

#?(:clj (defn page-identifier
          ([page]
           (let [entrypoint (key (first page))]
             (assert (seq? entrypoint) (str "A page entrypoint must be a route-like expression. Given `" (pr-str entrypoint) "`."))
             (identifier entrypoint)))
          ([&env page]
           (page-identifier (hfql/expand &env page)))))

#?(:clj
   (defn routing-map [&env pages]
     (->> pages
          (map (fn [page] [(list 'quote (page-identifier &env page)) `#'(hfql/hfql ~page)]))
          (into {}))))

(defn args-indices [f]
  (into {} (map-indexed (fn [idx arg] [(:name arg) idx])) (spec/args f)))

(defn fns-args-indices [fns] (reduce (fn [r f] (assoc r (list 'quote f) (args-indices f))) {} fns))

(defmacro arg-getter [indices arg route]
  `(let [[f# & args#] ~route]
     (if-let [index# (get-in ~indices [f# ~arg])]
       (when (< index# (count args#))
         (nth args# index#))
       (prn (ex-info "Unknown route arg" {:f f#
                                          :arg ~arg
                                          :indices ~indices})))))

(defmacro with-route-getters [route fns & body]
  (let [indices-sym (gensym "indices")
        indices     (fns-args-indices fns)
        all-args    (->> indices vals (mapcat keys) (into #{}))]
    `(let [~indices-sym ~indices
           ~@(mapcat (fn [arg] [(symbol arg) `(arg-getter ~indices-sym ~arg ~route)]) all-args)]
       ~@body)))

;;* Router
;;
;;  Takes:
;;  - a route, for the router to be driven from the outside,
;;  - one or more pages descriptions.
;;
;;  A page description is a single key-value HFQL map expression.
;;
;;  Example usage:
;;
;;  #+begin_src clojure
;;    ;; Router hooked to the browser
;;    (router route
;;            {(page1) [:db/id]}
;;            {(page2) [:db/id]})
;;  #+end_src
;;
#?(:clj (defmacro router [route & pages] ;; pages are HFQL exprs, they must all have a
          (assert (symbol? route))
          (validate-pages! pages)
          (let [env         (env/make-env &env)
                routing-map (routing-map env pages)
                fns         (set (map (partial page-identifier env) pages))]
            `(with-route-getters ~route ~fns
               (let [routing-map#  ~routing-map]
                 (validate-route! ~route)
                 (p/$ (xp/deduping (get routing-map# (first ~route) not-found))))))))

(p/defn not-found [] "page not found")
