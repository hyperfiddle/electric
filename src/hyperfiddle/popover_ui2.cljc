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

(p/defn BranchWrap [Body-client] ; todo colorless p/fns
  (p/server
    (let [stage (hf/branch (p/client (Body-client.))
                  hf/stage)]
      (p/client
        (dom/hr)
        (let [return (m/dfv)]
          (ui/button (p/fn [] (p/server (hf/Transact!. stage)) (return :commit)) (dom/text "commit!"))
          (ui/button (p/fn []
                       (return :discard)) (dom/text "discard"))
          (ui/edn stage nil (dom/props {::dom/disabled true
                                        ::dom/style {:display "block" :width "100%" :height "3rem"}}))
          (new (p/task->cp return)))))))

(p/defn PopoverBody [Body]
  (dom/div (dom/props {:class    "hyperfiddle popover-body"
                       :tabIndex "1"})
    (dom/event "click" (fn [e]
                         (when (= (.-target e) (.-currentTarget e)) ; click on self
                           (.focus (.-currentTarget e)))))
    (BranchWrap. (p/fn [] (Body.)))))

(p/defn Popover [label Body]
  (let [!open? (atom false), open? (p/watch !open?)]
    (dom/div (dom/props {:class "hyperfiddle popover-wrapper"})
      (ui/button (p/fn [] (swap! !open? not)) (dom/text label)) ; popover anchor
      (when open? (case (PopoverBody. Body) (swap! !open? not))))))

(defmacro staged [& body] `(new BranchWrap (p/fn [] ~@body)))
(defmacro popover [label & body] `(new Popover ~label (p/fn [] ~@body)))
(defmacro popover-staged [label & body] `(~'popover ~label (~'staged ~@body)))
