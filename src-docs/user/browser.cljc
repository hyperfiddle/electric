(ns user.browser
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui6 :as ui]
            [missionary.core :as m]
            ;; #?(:clj [hyperfiddle.q9 :refer [hfql]])
            [user.gender-shirt-size :refer [submissions genders submission shirt-sizes sub-profile]]
            [hyperfiddle.hfql.router :as router]
            [clojure.edn :as edn]
            [hyperfiddle.dev.logger :as log])
  #?(:cljs (:require-macros [user.browser :refer [view NavBar NotFoundPage]]
                            [user.gender-shirt-size :refer [submissions sub-profile]])))

;; NOTE
;; shirt-sizes computed for each row, should we cache? could the DAG ensures deduplication?

(p/defn NavBar []
  (dom/div
   (dom/class "navbar")
   (let [>route    (m/watch hf/route)
         route-str (p/$ ui/input
                        {:dom.property/value       (str (or ~>route `(submissions "alice")))
                         :dom.attribute/type       "text"
                         :dom.property/placeholder "(page …)"
                         :dom.property/class       "route"}
                        dom/target-value)
         route     (try (edn/read-string route-str)
                        (catch :default t
                          (hyperfiddle.dev.logger/error t)
                          nil))
         go!       (dom/button (dom/class "go-button")
                               ~(->> (dom/events dom/parent dom/click-event)
                                     ;; (m/eduction (map (constantly true)))
                                     (m/reductions {} false)
                                     (m/relieve {})))]
     (log/info "Route" hf/route "-> " ~>route "->" route)
     (if go!
       route
       '(about:homepage)))))

(p/defn NotFoundPage []
  ~@(let [route @hf/route]
      (cond
        (= route nil) nil
        :else         (dom/div
                       (dom/h1 (dom/text "Page not found"))
                       (dom/code (pr-str route))))))

(p/defn view []
  ~@(binding [hf/db "$"]
      ~@(dom/div
         (dom/class "browser")
         (let [route     (p/$ NavBar)
               #_#_x-ray (dom/input (dom/attribute "type" "checkbox")
                                    ~(->> (dom/events dom/parent "input")
                                          (m/eduction (map dom/target-checked))
                                          (m/reductions {} false)
                                          (m/relieve {})))]
           (log/info "Route" route) ;; force dom node order
           (dom/div
            (dom/class "view")
            ~@(p/$ ui/with-spec-render
                   #'(binding [hf/render        ui/render
                               hf/db            "$"
                               hf/route         (atom route)
                               router/not-found NotFoundPage]
                       (router/router
                        {(sub-profile 9) [:db/id]}
                        {(submissions . .)
                         [(props :db/id {::hf/link sub-profile})
                          :dustingetz/email
                          {(props :dustingetz/gender {::hf/options      (genders)
                                                        ::hf/option-label :db/ident
                                                        ::hf/render       ui/typeahead}) [(props :db/ident {::hf/as gender})]}
                          {(props :dustingetz/shirt-size {::hf/options      (shirt-sizes gender .)
                                                            ::hf/option-label :db/ident
                                                            #_#_::hf/render   ui/typeahead}) [:db/ident]}]}))))))))

(def !input (atom (list 9 10 11)))

(defn set-input [x]
  (reset! !input (clojure.edn/read-string x)))

#_(p/defn view []
  (let [ids ~(m/watch !input)]
    (dom/ul
     (p/for [id ids]
       (dom/li (dom/text (pr-str id)))))))

(def exports (p/vars !input))







(comment
  (reset! !input (list 9 10))
  )





(comment

  (def !input (atom (list 9 10 11)))

  (def dispose (p/run (prn (p/for [id ~(m/watch !input)]
                             id))))

  (reset! !input (list 9 10))



  (dispose)
  )
