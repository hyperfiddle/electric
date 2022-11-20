(ns dustin.y2022.state-machine-vs-callbacks
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.photon-ui :as ui]))

(p/defn Button [label busy]
  (dom/button {:aria-busy busy :disabled busy}
    label
    (dom/Event. "click" busy)))

(comment
  (p/with-cycle [busy false]
    (when-some [e (Button. "transact!" busy)]
      (p/server (d/transact! ...))))
  )

(p/defn Button [label busy]
  (dom/button {:aria-busy busy :disabled busy}
    label
    (when-some [e (dom/Event. "click" busy)]
      (F. e))))

(comment
  (p/with-cycle [busy false]
    (Button. "transact!" busy (p/fn [e]
                                (p/server (d/transact! ...)))))
  )