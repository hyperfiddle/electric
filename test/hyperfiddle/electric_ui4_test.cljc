(ns hyperfiddle.electric-ui4-test
  (:import [missionary Cancelled] [hyperfiddle.electric Pending])
  (:require
   #?(:cljs [contrib.dom-test-helpers :as uit])
   [contrib.cljs-target :refer [do-browser]]
   [hyperfiddle.electric :as p]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.rcf :as rcf :refer [% tests with tap]]
   [missionary.core :as m]))

#?(:cljs
   (do-browser
     (def !cv (atom 0))
     (def !in (atom nil))
     (def V-done? (m/dfv))
     (tests "setter waits for callback to finish"
       (with (e/run (try (binding [dom/node js/document.body]
                           (dom/input
                             (ui/control "input" ui/value identity (e/watch !cv)
                               (e/fn [v] (case (new (e/task->cp V-done?)) (tap [:V! v])))
                               #(tap [:setter %2])
                               (reset! !in dom/node))))
                         (catch Pending _)))
         % := [:setter 0]                ; initial set
         (uit/set-value! @!in "00")      ; we typed
         (swap! !cv inc)                 ; a new controlled value flowed in, V is awaiting the dfv
         (tap ::waiting), % := ::waiting ; check the queue is really empty
         (V-done? true)                  ; V finishes
         % := [:V! "00"]                 ; first V
         % := [:setter 1]                ; setter ran only after V resolved

         (tap ::done), % := ::done, (println " ok")))))
