(ns geoffrey.popover
  (:require [datascript.core :as d]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros geoffrey.popover)))


(p/def db)
(p/def stage!)

(def mappend (comp vec concat))

(p/defn Popover [Body]
  (let [!open (atom false)
        open?  (p/watch !open)
        !ret  (atom nil)]
    (ui/button {::ui/click-event (p/fn [_] (reset! !ret nil) (swap! !open not))}
      (dom/text (if open? "close" "open") " popover")) ; popover anchor
    (if-not open?                       ; popover body
      (p/watch !ret)
      (dom/div {:style {:border  "1px pink solid"
                        :padding "1rem"}} ; return tx on close
        (p/server
          (let [!stage (atom [])
                stage  (p/watch !stage)] ; fork
            (binding [#_#_db (d/with db stage)
                      stage! (partial swap! !stage mappend)]
              (p/client
                (Body.)               ; discard, popover commands are local
                (dom/hr)
                (ui/button {::ui/click-event (p/fn [e] (reset! !ret stage) (reset! !open false))} ; emit the stage on close
                  (dom/text "commit!"))
                (ui/button {::ui/click-event (p/fn [e] (reset! !open false))} ; emit the stage on close
                  (dom/text "cancel"))
                (dom/textarea {:style {:display :block}} (dom/text stage))))))))))

(p/defn App []
  (p/server
    (let [!stage (atom [])               ; root
          stage  (p/watch !stage)]
      (binding [#_#_db     (d/with db stage)]
        (p/client
          (dom/div
            (when-let [tx (Popover. (p/fn []
                                      (dom/p (dom/text "Popover body"))
                                      (ui/button {::dom/class      "stage"
                                                  ::ui/click-event (p/fn [_] (p/server (stage! [[:db/add 42 :foo/bar :baz]])))}
                                        (dom/text "Emit command"))))]
              (p/server (swap! !stage mappend tx)))
            (dom/p (dom/text "Parent stage"))
            (dom/textarea {:style {:display :block}} (dom/text (pr-str stage))))
          )))))


(def main #?(:cljs (p/boot (try (binding [dom/node (dom/by-id "root")] (App.)) (catch Pending _)))))

(comment
  (d/transact! !conn [[:db/retractEntity id]])
  (swap! !stage mappend [[:db/retractEntity id]])
  (stage! [[:db/retractEntity id]])
  `(stage! ~[[:db/retractEntity id]])
  `(stage! ~[:db/retractEntity id])
  (retractEntity! id)
  [[:db/retractEntity id]]
  `(d/transact! !conn ~[[:db/retractEntity id]])
  [::retractEntity id]
  )

                                        ; next steps - a hyper-button with pending, cancel, retry on a server command
