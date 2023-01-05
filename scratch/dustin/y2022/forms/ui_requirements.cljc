(ns dustin.y2022.forms.ui-requirements
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros dustin.y2022.forms.ui-requirements)))

; photon-dom2 - Leo dom, low level effects only
; photon-dom3 - keyword args and spread


; routing effects - command or loop?
; should photon-dom support click events?
(p/defn Nav-link [route label]
  (p/client
    (let [path (encode-path route)]
      (ui/element dom/a
        ::dom/href path
        ::ui/click-event (p/fn [e]
                           (.preventDefault e)
                           (router/pushState! !path path)) label))))


