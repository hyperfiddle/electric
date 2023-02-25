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
    (dom/event "click" (fn [e]
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

;; ----

(e/defn BranchWrap2 [Validate Transact Body-client] ; todo colorless p/fns
  (binding [hf/validation-hints (spec/reformat-explain-data (Validate.))]
    (e/server
      (hf/branch (e/client (Body-client.)) hf/stage)
      (e/client
        (dom/hr)
        (let [return (m/dfv)]
          (ui/button (e/fn [] (Transact. ) (return :commit))
            (when hf/validation-hints
              (dom/props {::dom/disabled true}))
            (dom/text "commit!"))
          (ui/button (e/fn []
                       (return :discard)) (dom/text "discard"))
          ;; TODO simplify this gymnastic
          (try (new (e/task->cp return)) ; Entrypoint treats pending as loading state which this is not
               (catch Pending _ nil)))))))

(e/defn PopoverBody2 [Validate Transact Body]
  (dom/div (dom/props {:class    "hyperfiddle popover-body"
                       :tabIndex "1"})
    (dom/event "click" (fn [e]
                         (when (= (.-target e) (.-currentTarget e)) ; click on self
                           (.focus (.-currentTarget e)))))
    (BranchWrap2. Validate Transact (e/fn [] (Body.)))))

(e/defn Popover2 [label Validate Transact Body]
  (let [!open? (atom false), open? (e/watch !open?)]
    (dom/div (dom/props {:class "hyperfiddle popover-wrapper"})
      (ui/button (e/fn [] (swap! !open? not)) (dom/text label)) ; popover anchor
      (when open?
        (case (PopoverBody2. Validate Transact Body)
          (:commit :discard) (swap! !open? not)
          nil                (do))
        nil))))

(defmacro popover2*
  ([label body]
   `(popover2* ~label (e/fn []) ~body))
  ([label Transact body]
   `(popover2* ~label (e/fn []) ~Transact ~body))
  ([label Validate Transact & body]
   `(e/client
     (router/router (router/proxy-history router/!history) ; sever popover state from URL
      (new Popover2 ~label ~Validate ~Transact (e/fn [] ~@body))))))

;; TODO Move to own namespace so we can retire popover1
(defmacro popover2 
  ([label HFQL-Expr]
   `(popover2 ~label (e/fn []) ~HFQL-Expr))
  ([label Transact HFQL-Expr]
   `(popover2 ~label (e/fn []) ~Transact ~HFQL-Expr))
  ([label Validate Transact HFQL-Expr]
   `(popover2* ~label ~Validate ~Transact
               (ttgui/with-gridsheet-renderer
                 (e/server
                  (hf/hfql [hf/*$* hf/db, suber.web.globals/*db* hf/db, hf/*nav!* hf/*nav!*]
                           ~HFQL-Expr))))))
