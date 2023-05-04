(ns hyperfiddle.popover
  #?(:cljs (:require-macros hyperfiddle.popover))
  (:import [hyperfiddle.electric Pending])
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [missionary.core :as m]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.history :as router]))

; data PopoverState = Closed | Open request | Pending request
; data BodyState = Idle | Request command | Pending command
; data Command = Commit tx | Discard

(e/defn BranchWrap [Body-client] ; todo colorless p/fns
  (e/server
    (let [stage (hf/branch (e/client (Body-client.))
                  hf/stage)]
      (e/client
        (dom/hr)
        (let [return (m/dfv)]
          (ui/button (e/fn [] (e/server (hf/Transact!. stage)) (return :commit)) (dom/text "commit!"))
          (ui/button (e/fn []
                       (return :discard)) (dom/text "discard"))
          (ui/edn stage nil (dom/props {::dom/disabled true
                                        ::dom/style {:display "block" :width "100%" :height "3rem"}}))
          (new (e/task->cp return)))))))

(e/defn PopoverBody [Body]
  (dom/div (dom/props {:class    "hyperfiddle popover-body"
                       :tabIndex "1"})
    (dom/on! "click" (fn [e]
                       (when (= (.-target e) (.-currentTarget e)) ; click on self
                         (.focus (.-currentTarget e)))))
    (BranchWrap. (e/fn [] (Body.)))))

(e/defn Popover [label Body]
  (let [!open? (atom false), open? (e/watch !open?)]
    (dom/div (dom/props {:class "hyperfiddle popover-wrapper"})
      (ui/button (e/fn [] (swap! !open? not)) (dom/text label)) ; popover anchor
      (when open? (case (PopoverBody. Body) (swap! !open? not))))))

(defmacro staged [& body] `(new BranchWrap (e/fn [] ~@body)))
(defmacro popover [label & body] `(new Popover ~label (e/fn [] ~@body)))
(defmacro popover-staged [label & body] `(~'popover ~label (~'staged ~@body)))

