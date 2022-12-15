(ns dustin.y2022.forms.ui-reqs
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.photon-ui :as ui]
            [hyperfiddle.rcf :as rcf :refer [tests tap % with]])
  #?(:cljs (:require-macros dustin.y2022.forms.ui-reqs)))



; Requirements:
; top level page queries database
; button: rename district
; button triggers popover
;   form in popover depends on dbval (query)
;   ui/input is pure FRP
;   flow of change used to build tx, or emit eav. need both
;   form that emits tx once valid to local stage
;   commit and discard
; reflow parent queries
; entrypoint bindings visible


; photon-dom2 - Leo dom, low level effects only, no sugar for text or props
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


; HFQL input controls (value edit)
; low level dom/input controls with events (blur, focus etc)
; fields / AV edit
; forms / EAV edit
; stage

; busy state, at page level and at field level(?)

; dom containers
; dd/dt pairs without containers, also do & let


; Work to do
; Pure CT demo-stage, no sugar for text or props, command monoid explicit
; Impure CT demo-stage, command monoid implicit, use kwarg sugar for props
; Neither of these photon-dom versions exist