(ns hyperfiddle.popover-ui2
  (:require [contrib.css :refer [css-slugify]]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom2 :as dom]
            [hyperfiddle.photon-ui3 :as ui])
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
        (let [commit (ui/button! (p/fn [] (p/server (hf/Transact!. stage)) ::close!) (dom/text "commit!"))
              discard (ui/button! (p/fn [] ::close!) (dom/text "discard"))]
          (ui/edn-editor (p/server hf/stage) {::dom/disabled true
                                              ::dom/style {:display "block" :width "100%" :height "3rem"}})
          (or commit discard))))))

(p/defn PopoverBody [Body]
  (dom/div (dom/props {:style {:position "fixed" :z-index "2"}})
    (dom/div (dom/props {:style {:border           "1px pink solid" :padding "5px"
                                 :position         "relative" #_#_#_#_:left "3em" :top "2em" :z-index "1"
                                 ;; :width            "fit-content" #_:height "40em"
                                 :background-color "rgb(248 250 252)"
                                 :box-shadow       "0 0 1rem lightgrey"}})
      (StageWrap. (p/fn [] (Body.))))))

(p/defn Popover [label Body]
  (p/with-cycle [status :closed]
    (let [toggle (when-some [event (ui/button false (dom/text label))] ; popover anchor
                   event)
          request (case status
                    :closed nil
                    (:open :pending) (PopoverBody. Body))] ; emit to close with request
      (println 'Popover toggle request)
      (case status
        :closed (if toggle :open :closed)
        :open (case request ; if request, transition to pending, then close
                nil (if toggle :closed :open)
                :pending)

        ; todo what happens if the parent load fails due to concurrent modification
        :pending (if false #_hf/loading :pending :closed)))) ; close when loading is finished
  nil)

(defmacro staged [& body] `(new StageWrap (p/fn [] ~@body)))
(defmacro popover [label & body] `(new Popover ~label (p/fn [] ~@body)))
(defmacro popover-staged [label & body] `(~'popover ~label (~'staged ~@body)))