(ns user.browser
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui6 :as ui]
            [missionary.core :as m]
            ;; #?(:clj [hyperfiddle.q9 :refer [hfql]])
            [user.gender-shirt-size :refer [submissions genders submission shirt-sizes sub-profile]]
            [hyperfiddle.hfql.router :as router]
            [hyperfiddle.ui.codemirror :as cm]
            [clojure.edn :as edn]
            [hyperfiddle.dev.logger :as log]
            [clojure.pprint :as pprint])
  #?(:cljs (:require-macros [user.browser :refer [view NavBar NotFoundPage]]
                            [user.gender-shirt-size :refer [submissions sub-profile]])))

;; NOTE
;; shirt-sizes computed for each row, should we cache? could the DAG ensures deduplication?

(p/defn NavBar []
  (dom/div
   (dom/class "navbar")
   (let [>route    (m/watch hf/route)
         default-route `(submissions "alice")
         route-str (p/$ cm/CodeMirror {:parent dom/parent, :inline true} default-route)
         #_(p/$ ui/input
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
     (do route ;; HACK
         (if go!
           (or route default-route)
           '(about:homepage))))))

(p/defn NotFoundPage []
  ~@(let [route @hf/route]
      (cond
        (= route nil) nil
        :else         (dom/div
                       (dom/h1 (dom/text "Page not found"))
                       (dom/code (pr-str route))))))

(defn pprint-str [x] (with-out-str (pprint/pprint x)))

(defn statement? [x] (and (vector? x)
                          (#{:db/add} (first x))))

(p/defn view []
  ~@(binding [hf/db "$"]
      ~@(dom/div
         (dom/class "browser")
         (let [route     (p/$ NavBar)]
           (do
             route ;; HACK
             (dom/div
              (dom/class "view")
              (let [tx ~@(p/$ ui/with-spec-render
                              #'(binding [hf/render        ui/render
                                          hf/db            "$"
                                          hf/route         (atom route)
                                          router/not-found NotFoundPage]
                                  (let [needle (nth route 1)]
                                    (router/router
                                     {(sub-profile 9) [:db/id]}
                                     {(submissions needle .)
                                      [(props :db/id {::hf/link sub-profile})
                                       :dustingetz/email
                                       {(props :dustingetz/gender {::hf/options      (genders)
                                                                   ::hf/option-label :db/ident
                                                                   ::hf/render       ui/typeahead}) [(props :db/ident {::hf/as gender})]}
                                       {(props :dustingetz/shirt-size {::hf/options      (shirt-sizes gender .)
                                                                       ::hf/option-label :db/ident}) [:db/ident]}]}))))]
                (dom/div
                 (dom/element "hr")
                 (p/$ cm/CodeMirror {:parent dom/parent} (vec (filter statement? (tree-seq coll? identity tx))))
                 #_(dom/code (dom/text (pprint-str tx)))))))))))


;; #_#_x-ray (dom/input (dom/attribute "type" "checkbox")
;;                      ~(->> (dom/events dom/parent "input")
;;                            (m/eduction (map dom/target-checked))
;;                            (m/reductions {} false)
;;                            (m/relieve {})))

;; (hyperfiddle.q9/hfql {(submissions . .) [:db/id]}) 

(def !input (atom "initial"))

(defn set-input [x] (reset! !input x))

#_(p/defn view []
  (log/info (p/$ cm/CodeMirror dom/parent ~(m/watch !input)))
  #_(p/for [id ~(m/watch !input)] ~(prn-up-down id)))

(comment ;; manually,  at the js REPL
  (set-input "(9 10 11)")
  (set-input "(9 10)")
  (set-input "(9 10 11 12)")
  )

(def exports (p/vars))







(comment
  (reset! !input (list 9 10))
  )





(comment

  (require '[hfdl.lang :as p])
  (require '[missionary.core :as m])
  (def !input (atom (list 9 10 11)))

  (defn prn-up-down [x] (m/observe (fn [!] (prn "up!" x) (! x) #(prn "down" x))))

  (def dispose (p/run (prn (p/for [id ~(m/watch !input)] ~(prn-up-down id)))))

  (reset! !input (list 9 10 11))

  (dispose)
  )
