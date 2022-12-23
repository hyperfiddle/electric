(ns dustin.y2022.ui.popover6
  #?(:cljs (:require-macros dustin.y2022.ui.popover6))
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
      (p/server
        (let [Transact!-old hf/Transact!]
          (let [stage (hf/branch
                        (p/client (Body-client.))
                        hf/stage)] ; propagate by ret channel, is this bad?
            (p/client
              (dom/hr)
              (let [commit (when (ui/Button. "commit!" false #_(not= hf/loading ::hf/idle))
                             (p/server (Transact!-old. stage))) ; todo doesn't close
                    discard (when (ui/Button. "discard" false #_(not= hf/loading ::hf/idle))
                              [])]
                (dom/element "style" (dom/text (str "." (css-slugify `stage) " { display: block; width: 100%; height: 10em; }")))
                (ui/edn-editor (p/server (doto stage (println 'stage))) {::dom/disabled true ::dom/class (css-slugify `stage)})
                (or commit discard)))))))))
