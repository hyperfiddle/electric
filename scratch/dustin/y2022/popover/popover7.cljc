(ns dustin.y2022.ui.popover7
  #?(:cljs (:require-macros dustin.y2022.ui.popover7))
  (:require [contrib.css :refer [css-slugify]]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.photon-ui2 :as ui])
  (:import [hyperfiddle.photon Pending]))

(p/defn PopoverBody [Body-client] ; todo colorless p/fns
  (let [!ret (atom nil)]
    (dom/div (dom/props {:style {:position "fixed"}})
      (dom/div (dom/props {:style {:border "1px pink solid" :padding "5px"
                                   :position "relative" :left "3em" :top "2em" :z-index "1"
                                   :width "50em" :height "40em"
                                   :background-color "rgb(248 250 252)"}})

        ; could photon-dom be the problem? No, it's hf/Transact
        ; could partial be the problem? No, factored out
        ; Current plan: simplify this to effectful interface, remove request.
        ; fix datomic issue by creating ui3/input, can't think clearly

        ; work skipping on the transact return value

        (p/server
          (hf/branch
            (p/client
              (Body-client.)
              (dom/hr)
              (when-let [commit (ui/Button. "commit!" false)]
                (p/server #_(when)
                  (let [x (hf/Transact!. hf/stage)] ; old-transact
                    (p/Unglitch. x)
                    (println 'PopoverBody-transact x)
                    (println 'PopoverBody-commit)
                    (p/client (reset! !ret []))))
                nil)

              (when-let [discard (ui/Button. "discard" false)]
                nil
                #_(reset! !ret []))

              (dom/element "style" (dom/text (str "." (css-slugify `stage) " { display: block; width: 100%; height: 10em; }")))
              (ui/edn-editor (p/server hf/stage) {::dom/disabled true ::dom/class (css-slugify `stage)})
              nil)))))
    (p/watch !ret)))
