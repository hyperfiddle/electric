(ns hyperfiddle.client.ui.demo
  (:require [hyperfiddle.client.ui :as ui :refer [tag text]]
            [missionary.core :as m]))

(defn root [>props]
  (let [!title (atom "Hyperfiddle UI")
        >title (m/watch !title)]
    (tag :div (m/latest (partial merge {"style" "border: 1px gray solid; margin: 1rem; padding: 1rem"})
                        >props)
         (tag :h1 nil (text >title))
         (tag :input (m/latest (partial assoc {"type"        "text"
                                               "placeholder" "Change title"
                                               "className"   "hf-cm-input"
                                               "onkeyup"     #(reset! !title (.. % -target -value))}
                                        "value")
                               >title)))))

(def !props (atom {}))
(def >props (m/watch !props))

(defn set-prop! [k v]
  (swap! !props assoc k v))

(defn remove-prop! [k]
  (swap! !props dissoc k))

(defn ^:export mount-root! []
  ((m/reactor
    (m/stream! (ui/mount-component-at-node! "hf-ui-dev-root" (root >props))))
   js/console.log js/console.error))

(defn ^:export unmount-root! [task]
  (task))

(defn ^:export stress!
  "Mount and unmount to trigger GC and track memory leaks"
  []
  (let [app (mount-root!)]
    (js/setTimeout (fn []
                     (unmount-root! app)
                     (stress))
                   500)))
