(ns hyperfiddle.client.ui.demo-dataflow
  (:require [hyperfiddle.client.ui :as ui]
            [hfdl.lang :as d :include-macros true]
            [hfdl.lib :refer [reactive-for]]
            [missionary.core :as m]
            [hyperfiddle.rcf :refer-macros [tests]]
            [edamame.core :as edn]))

(def sampler nil)

(defn define-sampler! [s]
  (js/console.log (case s nil "booting..." "operational."))
  (set! sampler s))

(defn set-needle! [!needle]
  (fn [^js e]
    (reset! !needle (.. e -target -value))))

(defn echo [] (d/dataflow
               (let [!needle (atom "foo")
                     >needle (m/watch !needle)]
                 (ui/tag :form nil
                         (ui/tag :input ~{:html/type     :text
                                          :html/class    "hf-cm-input"
                                          :html/value    @>needle
                                          :html/on-input (set-needle! !needle)})
                         (ui/tag :pre nil (ui/text >needle))))))

(declare edn-renderer)

(defn table-row [[k v]]
  (d/dataflow
   (ui/tag :<> nil
           (ui/tag :div nil (ui/text ~k))
           (ui/tag :div nil (doto @(edn-renderer (doto ~v prn)) prn)))))

(defn edn-renderer [>edn]
  (d/dataflow
   (let [edn @>edn]
     (cond
       (map? edn) (ui/tag :div ~{:html/style {:display "flex", :width "fit-content"}}
                          (ui/tag :div ~{:html/style {:display "flex"}}
                                  (ui/text ~"{"))
                          (apply ui/tag :div ~{:html/style {:display               "grid"
                                                            :grid-template-columns "auto 1fr"
                                                            :grid-gap              "0 0.25rem"}}
                                 @(reactive-for table-row ~edn))
                          (ui/tag :div ~{:html/style {:display "flex" :align-items "flex-end"}}
                                  (ui/text ~"}")))
       :else      (do
                    (prn 'text edn)
                    (ui/text ~edn))))))

(defn err->diagnostic [err]
  (let [{:keys [:type
                :edamame/expected-delimiter
                :edamame/opened-delimiter]} (ex-data err)
        message                             (ex-message err)]
    {:severity (case type
                 (:edamame/error :reader-exception)    "error"
                 type)
     :message  (cond
                 expected-delimiter (str "Expected " expected-delimiter
                                         " to match " opened-delimiter)
                 :else              message)}))

(defn validate-edn [^js e]
  (let [text    (.. e -target -parentNode -innerText)
        [_ err] (try [(edn/parse-string text) nil]
                     (catch js/Error e
                       [nil (err->diagnostic e)]))]
    (js/console.log 'validate-edn text err e)
    (if err
      (do
        (.stopPropagation e)
        (.. e -target -parentNode -classList (add "hf-invalid-input"))
        (.. e -target -parentNode (setAttribute "title" (:message err))))
      (.. e -target -parentNode -classList (remove "hf-invalid-input")))
    nil))


(declare edn-renderer)

(defn edn-key-value [[k v]]
  (d/dataflow
   (ui/tag :span nil
           (ui/tag :span nil (ui/text ~k))
           (ui/tag :span ~{#_#_:html/type                 :text
                           :html/content-editable         true
                           #_#_:html/value                v
                           :html/on-focus                 (partial js/console.log "focus")
                           :html/on-DOMCharacterDataModified validate-edn}
                   (ui/text ~v)
                   #_@(edn-renderer ~v)))))

(defn edn-renderer [>edn]
  (d/dataflow
   (let [edn @>edn]
     (cond
       (map? edn) (ui/tag :span ~{:html/content-editable true}
                          (ui/tag :span nil (ui/text ~"{"))
                          (apply ui/tag :span nil @(reactive-for edn-key-value >edn))
                          (ui/tag :span nil (ui/text ~"}")))
       :else      (ui/text ~edn)))))

(def ^:export main
  (d/client ;; ( -> _) -> (t -> Task void) -> Flow -> Task  ; client spawns a reactor, initialize a context
   (d/boot ;; (Derefable | nil -> _) -> Flow -> ( -> _)
    define-sampler!
    (d/dataflow
     @(ui/mount-component-at-node! "hf-ui-dev-root"
                                   ;; (ui/text @(foo ~[1 2 3]))
                                   @(edn-renderer ~{:a 1
                                                    :b {:c 2}}))
     nil))
   (fn [x] (m/sp (prn "fake WS put" x)))
   m/none ;; fake WS read, produces nothing, forever
   ))

;; (-> (tag :div)
;;     (prop :html/id ~"foo")
;;     (children (text ~"hello")))
