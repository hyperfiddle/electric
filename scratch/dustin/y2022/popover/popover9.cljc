(ns dustin.y2022.ui.popover9
  #?(:cljs (:require-macros dustin.y2022.ui.popover9))
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
        (let [stage (hf/branch (p/client (Body-client.))
                               hf/stage)]
          (p/client
            (dom/hr)
            (let [commit (ui/Button. "commit!" (not= hf/loading ::hf/idle))
                  discard (ui/Button. "discard" (not= hf/loading ::hf/idle))]
              (dom/element "style" (dom/text (str "." (css-slugify `stage) " { display: block; width: 100%; height: 10em; }")))
              (ui/edn-editor stage {::dom/disabled true ::dom/class (css-slugify `stage)})

              (println commit discard)
              (cond
                (some? commit) (do (p/server 1) [])
                (some? discard) (do (p/server 1) [])))))))))
