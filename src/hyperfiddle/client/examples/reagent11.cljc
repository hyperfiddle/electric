(ns hyperfiddle.client.examples.reagent11
  (:require [hfdl.lang :as photon]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros [hyperfiddle.client.examples.reagent11 :refer [TodoMVC Checkbox TodoItem TextInput]])))

(defn log [message x] #?(:cljs (js/console.log message x)) x)

(photon/defn Checkbox [done?]
  (dom/input (dom/attribute "type" "checkbox")
             (dom/property "checked" done?)
             ~(->> (dom/events dom/parent dom/change-event)
                   (m/eduction (map dom/event-target)
                               (map dom/get-checked)
                               (map boolean)
                               (map (partial log "Checkbox")))
                   (m/reductions {} done?)
                   (m/relieve {}))))

(photon/defn TextInput [text]
  (dom/input (dom/attribute "type" "text")
             (dom/attribute "value" text)
             ~(->> (dom/events dom/parent dom/input-event)
                   (m/eduction (map dom/event-target)
                               (map dom/get-value))
                   (m/reductions {} text)
                   (m/relieve {}))))

(photon/defn TodoItem [{:keys [done? text] :as task}]
  (dom/div (dom/attribute "data-done" (str done?))
           (let [done? (photon/$ Checkbox done?)
                 text (photon/$ TextInput text)]
             (assoc task :done? done?, :text text))))

(def example-state [{:id 1 :done? true   :text "foo"}
                    {:id 2 :done? false  :text "bar"}
                    {:id 3 :done? true   :text "baz"}])

(defn enter-key? [event]
  ;; https://keycode.info/
  (= "Enter" (.-key event)))

(defn create-new! [!temp-text _]
  ;; (js/console.log !temp-text e)
  (let [text @!temp-text]
    (reset! !temp-text "")
    {:id    (Math/abs (hash text))     ; FIXME
     :text  text
     :done? false}))

(photon/defn TodoMVC [state]
  (let [!temp-text (atom "")]
    (dom/div (dom/attribute "class" "todo-list")
             (dom/span (dom/text "+"))
             (let [>new-todo (dom/input (dom/attribute "type" "text")
                                       (dom/attribute "value" ~(m/watch !temp-text))
                                       (dom/attribute "placeholder" "What needs to be done?")
                                       (->> (dom/events dom/parent dom/input-event)
                                            (m/eduction (map dom/event-target)
                                                        (map dom/get-value)
                                                        (map (partial reset! !temp-text)))
                                            (m/reductions {} nil)
                                            (m/relieve {}))
                                       (->>  (dom/events dom/parent "keypress")
                                             (m/eduction (filter enter-key?)
                                                         (map (partial create-new! !temp-text)))
                                             (m/reductions {} nil)
                                             (m/relieve {})))
                   >edits (photon/for [item state]
                            (photon/$ TodoItem item))]
               (log "State" state)
               (log "Stage" (conj >edits >new-todo))))))

;; Does photon alters my domain  

(def root #?(:cljs (js/document.getElementById "root")))

(defonce process (atom nil))

(defn ^:export main []
  (photon/run
    (photon/binding [dom/parent root]
      (let [!state (atom example-state)]
        (reset! !state (photon/$ TodoMVC ~(m/watch !state)))))))


(defn ^:dev/after-load start! []
  (when-not @process
    (reset! process (main))))

(defn ^:dev/before-load stop! []
  (when-let [stop @process]
    (stop)
    (reset! process nil)))
