(ns user.form-typeahead-hfql-dom
  (:require [hfdl.impl.util :as u]
            [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.q4 :as q]
            [hyperfiddle.rcf :refer [tests ! %]]
            [missionary.core :as m]
            #?(:clj [user.gender-shirt-size :refer [submissions submission shirt-sizes]])
            [hyperfiddle.photon-dom :as dom])
  #?(:cljs (:require-macros [user.form-typeahead-hfql-dom :refer [page-submissions typeahead-select #_render-text render-list]]
                            [user.gender-shirt-size :refer [submission submissions shirt-sizes]]))
  )

(p/defn typeahead-select []
  ~@(dom/element "pre"
                 (dom/text
                  (str
                   [:input {:type :text :value ""}]
                   (into [:select {:selected ~(:db/ident ~hf/value)}]
                         (p/for [opt ~~(::hf/options hf/props)] ;; [{:db/ident :a} {:db/ident :b}]
                           [:option {:value ~(:db/ident ~opt)} ~(:db/ident ~opt)]))))))

;; (p/def render-text #'[:input {:type :text, :value hf/value}])

(p/def render-list #'(do ~@(let [x ~@~hf/value]
                             (prn x dom/parent)
                             #_(dom/li (dom/text (str ~@ ~hf/value))))
                         nil))


(p/defn page-submissions []
  (let [!needle (atom "")
        needle ~(m/watch !needle)]
    (dom/div
     (reset! !needle  (dom/input ~(->> (dom/events dom/parent "input")
                                       (m/eduction (map dom/target-value))
                                       (m/reductions {} "")
                                       (m/relieve {}))))
     (dom/ul
      (let [parent dom/parent]
        ~@(str (q/hfql [{(submissions needle) [(:db/id ::hf/render render-list)]}])))))))


;; (hyperfiddle.rcf/set-timeout! 2000)


;; TODO move to runtime tests

#?(:clj
   (do
     (tests
       "Two peers system"
       (p/run2 {} (! ~@ 1))
       % := 1)


     (p/def foo ::unbound)

     (tests
       "Not the same binding instance on client vs server"
       (p/run2 {} (! (p/binding [foo 1] ~@ foo)))
       % := ::unbound)

     (tests
       "Bindings are local to a peer, hopping between peers preserves bindings."
       ;; ⚠ run2 uses synchronous rdvs. ~@~@ happens in a single propagation frame.
       ;;   We need to change run2 such that it introduces asynchrony to make sure
       ;;   both ~@ and ~@ happens in different frames.
       (p/run2 {} (! (binding [foo 1] ~@ ~@ foo)))
       % := 1)
     ))

;; ------------------------



















#?(:clj
   (tests
     (p/run (! (-> (q/hfql [{(submissions "") [:db/id]
                             #_[:db/id (:dustingetz/email #_#_::hf/render render-text)
                                {(:dustingetz/shirt-size
                                  ::hf/options {(shirt-sizes :dustingetz/male "") [:db/ident]}
                                  #_#_::hf/render typeahead-select)
                                 [:db/ident]}]}]) )))
     %))


#_[{(submissions needle) [(:db/id ::hf/render render-list) :dustingetz/email]
  #_ [:db/id (:dustingetz/email #_#_::hf/render render-text)
      {(:dustingetz/shirt-size
        ::hf/options {(shirt-sizes :dustingetz/male "") [:db/ident]}
        #_#_::hf/render typeahead-select)
       [:db/ident]}]}]
