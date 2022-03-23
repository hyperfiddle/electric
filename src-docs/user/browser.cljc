(ns user.browser
  (:require [hfdl.lang :as p]
            [hfdl.lib :as lib]
            ;; #?(:clj [hfdl.lib :as lib :refer [forget deduping ]])
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui6 :as ui]
            [missionary.core :as m]
            ;; #?(:clj [hyperfiddle.q9 :refer [hfql]])
            [user.gender-shirt-size :refer [submissions genders submission shirt-sizes sub-profile]]
            [hyperfiddle.hfql.router :as router]
            [hyperfiddle.zero :as z]
            [hyperfiddle.ui.codemirror :as cm]
            [clojure.edn :as edn]
            [hyperfiddle.dev.logger :as log]
            [clojure.pprint :as pprint]
            #?(:clj [datahike.api :as d]))
  #?(:cljs (:require-macros [user.browser :refer [view NavBar NotFoundPage]]
                            [user.gender-shirt-size :refer [submissions sub-profile]]
                            [hfdl.lib :as lib :refer [forget deduping]])))

;; NOTE
;; shirt-sizes computed for each row, should we cache? could the DAG ensures deduplication?


(defn read-edn [edn-str]
  (try (edn/read-string edn-str)
       (catch #?(:clj Throwable :cljs :default) t
         (hyperfiddle.dev.logger/error t)
         nil)))

(defn write-edn [edn] (with-out-str (pprint/pprint edn)))

(p/defn NavBar [route]
  (dom/div
   (dom/class "navbar")
   (p/$ cm/CodeMirror {:parent dom/parent :inline true} read-edn write-edn route)))

(p/defn NotFoundPage []
  ~@(dom/div
     (dom/h1 (dom/text "Page not found"))
     (dom/code (dom/text (pr-str hf/route)))))
#?(:clj (defn transact!
          ([db stage] (transact! db stage false))
          ([db stage commit?]
           (try (let [db'
                      (if commit?
                        (throw (ex-info "Commit not implemented yet" {}))
                        (if (not-empty stage)
                          (let [{:keys [tempids db-after]} (d/with (:db db) {:tx-data stage})]
                            (-> (assoc db :tempids tempids :db db-after)
                                (update :basis-t (fnil inc 0))))
                          db))]
                  (prn "DB =>" db')
                  (prn "stage =>" stage)
                  [db' nil])
                (catch Throwable t
                  [db (ex-message t)]))))
   :cljs (defn transact! [& _] (throw (ex-info "Server side only" {}))))

(defn transact!! [!db !stage _]
  (let [db    @!db
        stage @!stage
        [db' _] (transact! db stage)]
    (when (not= db db')
      (reset! !stage nil)
      (reset! !db db'))
    nil))

(defn set-route! [_event]
  (reset! hf/route-state `(sub-profile ~(rand-int 100))))

(p/defn view []
  ~@;; server
    (let [!db          (atom (hf/->DB "$" 0 nil hf/*$*))
          >db          (m/watch !db)
          !stage       (atom nil)
          stage        ~(m/watch !stage)
          [db message] (transact! ~>db stage)]
      (binding [hf/db (deduping db)]
        ~@;; client
          (dom/div
           (dom/class "browser")
           (binding [hf/route (p/$ NavBar (or ~(m/watch hf/route-state)
                                              `(user.gender-shirt-size/sub-profile 10)))]
             hf/route ;; hack
             (dom/div
              (dom/class "view")
              (let [route hf/route ;; TODO binding unification
                    tx    ~@(ui/with-spec-render
                              (binding [hf/render        ui/render
                                        router/not-found NotFoundPage]
                                (let [[_ sub] route]
                                  (router/router
                                   route
                                   {(sub-profile sub) [:db/id :dustingetz/email]}
                                   {(submissions . .) [(props :db/id {::hf/link sub-profile})
                                                       :dustingetz/email
                                                       {(props :dustingetz/gender {::hf/options      (genders)
                                                                                   ::hf/option-label :db/ident
                                                                                   ::hf/render       ui/select-options}) [(props :db/ident {::hf/as gender})]}
                                                       {(props :dustingetz/shirt-size {::hf/options      (shirt-sizes gender .)
                                                                                       ::hf/option-label :db/ident}) [:db/ident]}]}))))]
                (dom/div (dom/class "hf-error-wrapper")
                 #_(dom/p (dom/text (str "Query result: " ~@~(hf/q '[:find ?email . :where [9 :dustingetz/email ?email]]
                                                                   (:db hf/db)))))
                 #_(p/$ cm/CodeMirror {:parent dom/parent} ~@(assoc hf/db :db "<server side only>"))
                 
                 (let [tx' (p/$ cm/CodeMirror {:parent dom/parent} read-edn write-edn [])]
                   (do tx'
                       ~@(forget (reset! !stage tx'))

                   ;; TODO use z/fsm or z/instant
                       #_(let [click (dom/button (dom/text "transact!")
                                               ~(->> (dom/events dom/parent "click")
                                                     (m/eduction (map (constantly true)))
                                                     (ui/continuous)))]
                         ~@(forget ~(->> #'click
                                         (m/eduction (filter boolean?)
                                                     (map (partial transact!! !db !stage)))
                                         (ui/continuous))))
                       (dom/code (dom/class "hf-error") 
                                 (dom/style {"margin" "1rem 0"})
                                 (dom/text message))))))))))))

(def exports (p/vars transact! ui/continuous ui/debounce not-empty boolean? transact!!))


(comment

  (require '[hfdl.lang :as p])
  (require '[missionary.core :as m])
  (def !input (atom (list 9 10 11)))

  (defn prn-up-down [x] (m/observe (fn [!] (prn "up!" x) (! x) #(prn "down" x))))

  (def dispose (p/run (prn (p/for [id ~(m/watch !input)] ~(prn-up-down id)))))

  (reset! !input (list 9 10 11))

  (dispose))
