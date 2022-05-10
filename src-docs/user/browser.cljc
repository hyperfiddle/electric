(ns user.browser
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.ui :as ui]
            [missionary.core :as m]
            [user.orders :refer [orders genders shirt-sizes one-order]]
            [hyperfiddle.hfql.router :as router]
            [hyperfiddle.zero :as z]
            [hyperfiddle.ui.codemirror :as cm]
            #?(:clj [datahike.api :as d])))

;; NOTE
;; shirt-sizes computed for each row, should we cache? could the DAG ensures deduplication?

(p/defn NavBar []
  (let [route-state (new (m/watch hf/route-state))]
    (dom/div (dom/class "navbar")
             (ui/link (second route-state) hf/navigate-back! (dom/text "< back"))
             (dom/div (dom/class "navbar-route")
                      (cm/CodeMirror. {:parent dom/parent :inline true} cm/read-edn cm/write-edn (first route-state))))))

(p/defn NotFoundPage []
  ~@(dom/div
     (dom/h1 (dom/text "Page not found"))
     (dom/code (dom/text (pr-str hf/route)))))

#_(defn transact!! [!db !stage _] ;; TODO persist stagging area
  (let [db    @!db
        stage @!stage
        [db' _] (hf/transact! db stage)]
    (when (not= db db')
      (reset! !stage nil)
      (reset! !db db'))
    nil))

;; Set initial route state
#?(:cljs (reset! hf/route-state `((user.orders/orders "alice"))))

(p/defn View []
  ~@;; server
    (let [!stage       (atom nil)
          !db          (atom hf/*db*)
          stage        (p/Watch. !stage)
          db           (p/Watch. !db)
          [db message] (hf/transact! db stage)]
      (binding [hf/db (p/deduping db)]
        ~@;; client
          (binding [hf/route (NavBar.)]
            hf/route ;; hack
            (let [route hf/route ;; TODO binding unification
                  tx    ~@(ui/with-spec-render
                            (binding [hf/render        ui/render
                                      router/not-found NotFoundPage]
                              (let [[_ sub] route]
                                (router/router
                                 route
                                 {(one-order sub) [:db/id :order/email]}
                                 {(orders .) [(props :db/id {::hf/link one-order})
                                              :order/email
                                              {(props :order/gender {::hf/options      (genders)
                                                                     ::hf/option-label :db/ident
                                                                     ::hf/render       ui/select-options}) [(props :db/ident {::hf/as gender})]}
                                              {(props :order/shirt-size {::hf/options      (shirt-sizes gender .)
                                                                         ::hf/option-label :db/ident}) [:db/ident]}]}))))]
              (dom/div (dom/class "hf-staging-area")
                       (dom/div (dom/class "hf-error-wrapper")
                                (let [tx' (cm/CodeMirror. {:parent dom/parent} cm/read-edn cm/write-edn [])]
                                  (do tx'
                                      ~@(p/forget (reset! !stage tx'))
                                        ;; TODO use z/fsm or z/instant
                                      #_(let [click (dom/button (dom/text "transact!")
                                                                (new (->> (dom/events dom/parent "click")
                                                                          (m/eduction (map (constantly true)))
                                                                          (xp/continuous))))]
                                          ~@(xp/forget (new (->> (p/fn [] click)
                                                                 (m/eduction (filter boolean?)
                                                                             (map (partial transact!! !db !stage)))
                                                                 (xp/continuous)))))
                                      (dom/code (dom/class "hf-error")
                                                (dom/style {"margin" "1rem 0"})
                                                (dom/text message)))))))))))
