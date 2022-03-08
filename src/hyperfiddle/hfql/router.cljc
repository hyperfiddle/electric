(ns hyperfiddle.hfql.router
  (:require
   [hfdl.lang :as p]
   [hyperfiddle.q9 :refer [hfql]]
   [hyperfiddle.api :as hf]
   #?(:clj [hyperfiddle.q9.env :as env]))
  #?(:cljs (:require-macros [hyperfiddle.hfql.router :refer [set-route! not-found]])))

(defn fncall [sexpr] (and (seq? sexpr) (symbol? (first sexpr))))

(defn validate-route!
  ([route]
   (when (not (fncall route))
     (throw (ex-info (str "Tryed to set an invalid route. Routes are symbolic function calls like `(foo :bar)`, given `" route "`.") {:route route}))))
  ([old-route new-route]
   (let [ex-data {:old-route old-route
                  :new-route new-route}]
     (cond
       ;; (nil? old-route)         (throw (ex-info (str "Not in a routing context. Tried to set route `" (pr-str new-route) "`.") ex-data))
       (not (fncall new-route)) (throw (ex-info (str "Tryed to set an invalid route. Routes are symbolic function calls like `(foo :bar)`, given `" new-route "`.") ex-data))))))

(defn validate-pages! [pages]
  (assert (seq pages) "A router requires at least one page to route to.")
  (doseq [page pages]
    (assert (map? page) (str "A router can only route to a page. A page is a single key-value HFQL map. Given `" (pr-str page) "`"))
    (let [keys (keys page)]
      (assert (= 1 (count keys )) (str "A routable page must have a single entrypoint (root). The given page declares " (count keys) " entrypoints: " (pr-str keys) ". Please choose one. In `" (pr-str page) "`.")))))

(defn page-identifier [page]
  (let [entrypoint (key (first page))]
    (assert (seq? entrypoint) (str "A page entrypoint must be a route-like expression. Given `" (pr-str entrypoint) "`."))
    (first entrypoint)))

#?(:clj
   (defn routing-map [&env pages]
     (into {} (map (fn [page] [(list 'quote (env/resolve-syms &env (page-identifier page))) `#'(hfql ~page)])) pages)))

;;* Router
;;
;;  Takes:
;;  - a flow of routes, for the router to be driven from the outside,
;;  - a function called with the selected route,
;;  - one or more pages descriptions.
;;
;;  A page description is a single key-value HFQL map expression.
;;
;;  Example usage:
;;
;;  #+begin_src clojure
;;    ;; Router hooked to the browser
;;    (router >parsed-url set-url!
;;            {(page1) [:db/id]}
;;            {(page2) [:db/id]})
;;
;;    ;; Standalone router
;;    (router #'nil (constantly nil)
;;            {(page1) [:db/id]}
;;            {(page2) [:db/id]})
;;  #+end_src
;;
(defmacro router [& pages] ;; pages are HFQL exprs, they must all have a
  (validate-pages! pages)
  `(let [pages#  ~(routing-map (env/make-env &env) pages)
         >route# (m/watch hf/route)
         route#  (unquote >route#)]
     (validate-route! route#)
     (let [page# (get pages# (first route#))]
       (prn "Router Found" page#)
       (if (some? page#)
         (p/$ page#)
         (do (prn "Page not found" (pr-str (first route#)) "in" (pr-str (keys pages#)))
             (p/$ not-found))))))


(p/defn set-route! [sexpr]
  (validate-route! hf/route sexpr)
  (reset! hf/route sexpr))

(p/defn not-found [] "page not found")

;; (router #'nil (constantly nil)
;;         {(user.gender-shirt-size/submissions .) [:dustingetz/email]} ) 
