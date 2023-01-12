(ns hyperfiddle.popover-ui2
  (:require [contrib.css :refer [css-slugify]]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.photon-ui4 :as ui]
            [missionary.core :as m])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros hyperfiddle.popover-ui2)))

; data PopoverState = Closed | Open request | Pending request
; data BodyState = Idle | Request command | Pending command
; data Command = Commit tx | Discard

(p/defn StageWrap [Body-client] ; todo colorless p/fns
  (p/server
    (let [stage (hf/branch (p/client (Body-client.))
                           hf/stage)]
      (p/client
        (dom/hr)
        (let [return (m/dfv)]
          (ui/button (p/fn [] (p/server (hf/Transact!. stage)) (return :commit)) (dom/text "commit!"))
          (ui/button (p/fn [] (return :discard)) (dom/text "discard"))
          (ui/edn stage nil (dom/props {::dom/disabled true
                                        ::dom/style {:display "block" :width "100%" :height "3rem"}}))
          (new (p/task->cp return)))))))

(p/defn PopoverBody [Body]
  ;; with position fixed if mounts outside view can't scroll down to commit/discard
  (dom/div (dom/props {:style {#_ #_:position "fixed" :z-index "2"}})
    (dom/div (dom/props {:style {:border           "1px pink solid" :padding "5px"
                                 :position         "relative" #_#_#_#_:left "3em" :top "2em" :z-index "1"
                                 ;; :width            "fit-content" #_:height "40em"
                                 :background-color "rgb(248 250 252)"
                                 :box-shadow       "0 0 1rem lightgrey"}})
      (StageWrap. (p/fn [] (Body.))))))

(p/defn Popover [label Body]
  (let [!open? (atom false), open? (p/watch !open?)]
    (ui/button (p/fn [] (swap! !open? not)) (dom/text label)) ; popover anchor
    (when open? (case (PopoverBody. Body) (swap! !open? not)))))

(defmacro staged [& body] `(new StageWrap (p/fn [] ~@body)))
(defmacro popover [label & body] `(new Popover ~label (p/fn [] ~@body)))
(defmacro popover-staged [label & body] `(~'popover ~label (~'staged ~@body)))
