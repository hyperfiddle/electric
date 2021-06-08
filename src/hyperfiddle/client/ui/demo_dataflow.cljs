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
                         (ui/tag :input ~{:dom.attribute/type      :text
                                          :dom.attribute/className "hf-cm-input"
                                          :dom.attribute/value     @>needle
                                          :dom.event/input         (set-needle! !needle)})
                         (ui/tag :pre nil (ui/text >needle))))))

(declare edn-renderer)

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
  (let [text    (.. e -currentTarget -textContent)
        [_ err] (try [(edn/parse-string text) nil]
                     (catch js/Error e
                       [nil (err->diagnostic e)]))]
    (js/console.log 'validate-edn text err e)
    (if err
      (do
        (.. e -currentTarget (setAttribute "aria-invalid" true))
        (.. e -currentTarget (setAttribute "title" (:message err))))
      (.. e -currentTarget (removeAttribute "aria-invalid")))
    nil))


(declare edn-renderer)

(defn edn-key-value [[k v]]
  (d/dataflow
   (ui/tag :span ~{:dom.attribute/contentEditable true} ;; kv
           (ui/tag :span ~{:dom.attribute/contentEditable false} ;; protect k and v deletion
                   (ui/tag :span ~{:dom.attribute/contentEditable      true
                                   :dom.attribute/placeholder          "key"
                                   :dom.event/DOMCharacterDataModified validate-edn}
                           (ui/text ~k))
                   (ui/tag :span ~{#_#_:dom.attribute/type             :text
                                   :dom.attribute/contentEditable      true
                                   :dom.attribute/placeholder          "value"
                                   #_#_:dom.attribute/value            v
                                   :dom.event/focus                    (partial js/console.log "focus")
                                   :dom.event/DOMCharacterDataModified validate-edn}
                           (ui/text ~v)
                           #_@(edn-renderer ~v))))))

(defn edn-renderer [>edn]
  (d/dataflow
   (let [edn @>edn]
     (cond
       (map? edn) (ui/tag :span ~{:dom.attribute/contentEditable      true
                                  :dom.event/DOMCharacterDataModified validate-edn}
                          (ui/tag :span ~{:dom.attribute/contentEditable false}
                                  (ui/tag :span ~{:dom.attribute/contentEditable true} (ui/text ~"{"))
                                  (apply ui/tag :span nil @(reactive-for edn-key-value >edn))
                                  (ui/tag :span ~{:dom.attribute/contentEditable true} (ui/text ~"}"))))
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
;;     (prop :dom.attribute/id ~"foo")
;;     (children (text ~"hello")))
