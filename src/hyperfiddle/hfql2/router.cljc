(ns hyperfiddle.hfql2.router
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.spec :as spec]
   #?(:clj [shadow.resource :as res])
   #?(:clj [edamame.core :as edn])
   #?(:clj [hyperfiddle.hfql2 :as hfql])
   #?(:clj [hyperfiddle.photon-impl.compiler :as c])
   #?(:clj [hyperfiddle.logger :as log]))
  #?(:cljs (:require-macros [hyperfiddle.hfql2.router])))

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

#?(:clj
   (defn- identifier [page]
     (if-let [ident (loop [form page]
                      (cond
                        (map? form)    (recur (key (first form)))
                        (seq? form)    (if (= 'props (first form))
                                         (recur (second form))
                                         (recur (first form)))
                        (symbol? form) form
                        :else          nil))]
       ident
       (throw (ex-info  "Malformed page definition. It should be in the form {(page …) […]}" {:page page})))))

#?(:clj
   (defn page-identifier
     [env page]
     (let [ident (identifier page)]
       (if-let [var (c/resolve-var env ident)]
         (c/var-name var)
         (throw (ex-info (str "Unable to resolve page name `" ident "`") {:page page}))))))

#?(:clj
   (defn routing-map [env pages]
     (->> pages
          (map (fn [page] [(list 'quote (page-identifier env page)) `(p/fn [] (hfql2/hfql ~page))]))
          (into {}))))

(defn args-indices [f]
  (into {} (map-indexed (fn [idx arg] [(:name arg) idx])) (spec/args f)))

(defn fns-args-indices [fns] (reduce (fn [r f] (assoc r (list 'quote f) (args-indices f))) {} fns))

(defmacro arg-getter [indices arg route]
  `(let [[f# & args#] ~route]
     (if-let [index# (get-in ~indices [f# ~arg])]
       (when (< index# (count args#))
         (nth args# index#))
       (log/debug "Unknown route arg" {:f f# :arg ~arg :indices ~indices}))))

(defmacro with-route-getters [route fns & body]
  (let [indices-sym (gensym "indices")
        indices     (fns-args-indices fns)
        all-args    (->> indices vals (mapcat keys) (into #{}))]
    `(let [~indices-sym ~indices
           ~@(mapcat (fn [arg] [(symbol arg) `(arg-getter ~indices-sym ~arg ~route)]) all-args)]
       ~@body)))

#?(:clj (defn load-page [env page]
          (let [opts {:auto-resolve (assoc (-> env :ns :requires) :current (-> env :ns :name))}]
            (if (string? page)
              (edn/parse-string-all (res/slurp-resource env page) opts)
              [page]))))

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
(defmacro router [route & pages]
  (let [pages (mapcat (partial load-page &env) pages)]
    (validate-pages! pages)
    (let [env         (c/normalize-env &env)
          routing-map (routing-map env pages)
          fns         (set (map (partial page-identifier env) pages))]
      `(let [route# ~route]
         (with-route-getters route# ~fns
           (let [routing-map# ~routing-map]
             (validate-route! route#)
             (new (get routing-map# (first route#) not-found))))))))

(p/defn not-found [] "page not found")

