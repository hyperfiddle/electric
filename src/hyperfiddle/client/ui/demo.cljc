(ns hyperfiddle.client.ui.demo
  (:require [hyperfiddle.client.ui :as ui :refer [tag text]]
            [missionary.core :as m]))

(defn root [>needle]
  (let [!title (atom "Hyperfiddle UI")
        >title (m/watch !title)]
    (tag :div {"style" "border: 1px gray solid; margin: 1rem; padding: 1rem"}
         (tag :h1 nil (text >title))
         (tag :pre nil (text >needle))
         (tag :input (m/latest (partial assoc {"type"        "text"
                                               "placeholder" "Change title"
                                               "className"   "hf-cm-input"
                                               "onkeyup"     #(reset! !title (.. % -target -value))}
                                        "value")
                               >title)))))

(def !needle (atom ""))
(def >needle (m/watch !needle))

(defn ^:export mount-root! []
  ((m/reactor
    (ui/mount-component-at-node! "hf-ui-dev-root" (root >needle)))
   prn prn))
