(ns dustin.y2022.stage
  (:require [datascript.core :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.stage)))


(p/def db)
(p/def stage)
(p/def stage!)

(p/defn Popover [Body]
  (p/client
    (let [!open (atom false)]
      (if-not (p/watch !open)

        ; popover anchor
        (dom/button {::ui/click-event (p/fn [e] (reset! !open true) nil)} (dom/text "open popover"))

        ; popover body
        (dom/div                                          ; return tx on close
            (dom/h1 (dom/text "popover body"))
            (p/server
              (let [!stage (atom stage) stage (p/watch !stage)] ; fork
                (binding [db (d/with db stage)
                          stage! (partial swap! !stage concat)]
                  (p/client
                    (Body.)                                 ; discard, popover commands are local
                    (dom/button
                      {::ui/click-event (p/fn [e] (reset! !open false) stage)} ; emit the stage on close
                      (dom/text "commit!")))))))))))

(def !conn #?(:clj (d/create-conn {})))



(p/defn App []
  (p/server
    (let [!stage (atom []) stage (p/watch !stage)]          ; root
      (binding [db (d/with (p/watch !conn) stage)
                stage! (partial swap! !stage concat)]
        (p/client
          ; ...
          (dom/h1 (dom/text "hello world"))
          (when-let [tx (Popover. (p/fn []
                                    ;(p/client)
                                    ;(dom/p (dom/text (doto (apply str (p/server (d/q ... db))))))
                                    (p/for [id (p/server (d/q '[:find [?e ...] :in $ :where [?e :task/status]] db))]
                                      (p/client
                                        (ui/button {::dom/class      "destroy"
                                                    ::ui/click-event (p/fn [_]
                                                                       (p/server (stage! [[:db/retractEntity id]])))})))))]
            (stage! tx))

          (dom/h1 (dom/text "Staging area"))
          (dom/pre (dom/text (pr-str stage)))
          (ui/button {::ui/click-event (p/fn [e] (d/transact! !conn stage))} (dom/text "transact!")))))))

(def main
  #?(:cljs (p/boot
             (try (binding [dom/node (dom/by-id "root")]
                    (App.))
                  (catch Pending _)))))
