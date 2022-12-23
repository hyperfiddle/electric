(ns dustin.y2022.ui.popover8
  #?(:cljs (:require-macros dustin.y2022.ui.popover8))
  (:require [contrib.css :refer [css-slugify]]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  (:import [hyperfiddle.photon Pending]))

(p/defn PopoverBody [Body-client] ; todo colorless p/fns
  (dom/div (dom/props {:style {:position "fixed"}})
    (dom/div (dom/props {:style {:border "1px pink solid" :padding "5px"
                                 :position "relative" :left "3em" :top "2em" :z-index "1"
                                 :width "50em" :height "40em"
                                 :background-color "rgb(248 250 252)"}})

      ; could photon-dom be the problem? No, it's hf/Transact
      ; could partial be the problem? No, factored out
      ; work skipping on the transact return value? no
      ; Current plan: simplify this to effectful interface, remove request.
      ; fix datomic issue by creating ui3/input, can't think clearly

      ; transactional things by effect (swap!)
      ; other things by value, if not at risk of concurrent edit

      ; how to make progress?
      ; stop spamming datomic, that has to go anyway

      (let [[stage _ commit discard] [(p/server (hf/branch (p/client (Body-client.))
                                                           hf/stage))
                                      (dom/hr)
                                      (ui/Button. "commit!" false)
                                      (ui/Button. "discard" false)]]
        (dom/element "style" (dom/text (str "." (css-slugify `stage) " { display: block; width: 100%; height: 10em; }")))
        (ui/edn-editor (p/server stage) {::dom/disabled true ::dom/class (css-slugify `stage)})
        (or #_(and commit (let [_ (println "PopoverBody committing: " stage)
                                x true #_(p/server true #_(hf/Transact!. stage))
                                _ (println "PopoverBody commit result: " x)]
                            []))
          (and commit (do (println "PopoverBody committing: " stage)
                          ;(p/server true #_(hf/Transact!. stage))
                          (println "PopoverBody commit result: " #_x)
                          []))
          (and discard []))))))
