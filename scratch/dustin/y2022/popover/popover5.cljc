(ns dustin.y2022.ui.popover5
  (:require [contrib.css :refer [css-slugify]]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros dustin.y2022.ui.popover5)))

(p/defn PopoverBody [Body-client] ; todo colorless p/fns
  (dom/div (dom/props {:style {:position "fixed"}})
    (dom/div (dom/props {:style {:border "1px pink solid" :padding "5px"
                                 :position "relative" :left "3em" :top "2em" :z-index "1"
                                 :width "50em" :height "40em"
                                 :background-color "rgb(248 250 252)"}})
      (p/server
        ; save old Transact!
        (hf/branch
          (p/client
            (Body-client.)
            (dom/hr)
            ;; TODO disable when invalid
            (let [commit (when (ui/Button. "commit!" false #_(not= hf/loading ::hf/idle))
                           ; use old transact
                           (p/server (doto hf/stage (println 'commit)))) ; todo binding unification
                  discard (when (ui/Button. "discard" false #_(not= hf/loading ::hf/idle))
                            (println 'discard)
                            [])]
              (println 'commit commit)
              (dom/element "style" (dom/text (str "." (css-slugify `stage) " { display: block; width: 100%; height: 10em; }")))
              (ui/edn-editor (p/server hf/stage) {::dom/disabled true ::dom/class (css-slugify `stage)})
              (doto (or commit discard) (println 'PopoverBody-request)))))))))
