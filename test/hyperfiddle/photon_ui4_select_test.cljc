(ns hyperfiddle.photon-ui4-select-test
  #?(:cljs (:require-macros hyperfiddle.photon-ui4-select-test))
  (:require
   #?(:cljs [hyperfiddle.ui.test :as uit])
   [hyperfiddle.photon-dom :as dom1]
   [hyperfiddle.photon-ui4 :as ui]
   [hyperfiddle.rcf :as rcf :refer [% tests with tap]]
   [hyperfiddle.photon :as p]
   [clojure.string :as str]
   [hyperfiddle.photon-dom2 :as dom])
  (:import
   [missionary Cancelled]
   [hyperfiddle.photon Pending]))

(def data {:alice   {:name "Alice B"}
           :bob     {:name "Bob C"}
           :charlie {:name "Charlie D"}
           :derek   {:name "Derek E"}})
(defn q [] [:alice :bob :charlie :derek])

#?(:cljs (defn get-input [select] (-> select (.getElementsByTagName "input") first)))
#?(:cljs (defn get-option [select s] (some #(when (= s (.-innerText %)) %) (.querySelectorAll select "ul > div"))))

#?(:cljs
   (tests "basic behavior"
     (def !select (atom :missing))
     (def discard (p/run (try
                           (binding [dom1/node (dom1/by-id "root")]
                             (p/server
                               (let [!v (atom :alice)]
                                 (ui/select (p/watch !v)
                                   (p/fn [v] (p/client (tap [:V! v])) (reset! !v v))
                                   (p/fn [] (p/client (tap [:Options])) (q))
                                   (p/fn [id] (p/client (tap [:OptionLabel id])) (-> data id :name))
                                   #_for-test (reset! !select dom1/node)))))
                           (catch Pending _)
                           (catch Cancelled _)
                           (catch :default e (prn e)))))

     "initially OptionLabel runs on the controlled value"
     % := [:OptionLabel :alice]

     (def select @!select)
     (def input (get-input select))
     (some? input) := true

     "input has the correct value"
     (.-value input) := "Alice B"

     "when focused Options runs along with OptionLabel for each result"
     (uit/focus input)
     % := [:Options]
     (hash-set % % % %) := #{[:OptionLabel :alice]
                             [:OptionLabel :bob]
                             [:OptionLabel :charlie]
                             [:OptionLabel :derek]}

     "typing has no effect TODO"

     "when we click on an option V! runs, OptionLabel recalculates the new string and puts it into the input"
     (uit/click (get-option select "Charlie D"))
     % := [:V! :charlie]
     % := [:OptionLabel :charlie]
     (.-value input) := "Charlie D"

     (discard)
     ))


#?(:cljs
   (tests "controlled value"
     (def !select (atom :missing))
     (def !v (atom :alice))
     (def discard (p/run (try
                           (binding [dom1/node (dom1/by-id "root")]
                             (let [v (p/watch !v)]
                               (tap [:controlled-value v])
                               (p/server
                                 (ui/select v
                                   (p/fn [v] (reset! !v v))
                                   (p/fn [] (q))
                                   (p/fn [id] (tap [:OptionLabel id]) (-> data id :name))
                                   #_for-test (reset! !select dom1/node)))))
                           (catch Pending _)
                           (catch Cancelled _)
                           (catch :default e (prn e)))))

     "initially OptionLabel runs on the controlled value"
     % := [:controlled-value :alice]
     % := [:OptionLabel :alice]

     (def select @!select)
     (def input (get-input select))
     (some? input) := true

     "input has the correct value"
     (.-value input) := "Alice B"

     "when focused OptionLabel runs for each result"
     (uit/focus input)
     (hash-set % % % %) := #{[:OptionLabel :alice]
                             [:OptionLabel :bob]
                             [:OptionLabel :charlie]
                             [:OptionLabel :derek]}

     "while picking controlled value changes have no effect"
     (reset! !v :bob)
     % := [:controlled-value :bob]      ; no OptionLabel call here

     "after picking we see new value flow through"
     (uit/click (get-option select "Charlie D"))
     % := [:controlled-value :charlie]
     % := [:OptionLabel :charlie]

     (discard)
     ))

#?(:cljs
   (tests "close when clicked outside"
     (def !select (atom :missing))
     (def discard (p/run (try
                           (binding [dom1/node (dom1/by-id "root")]
                             (dom/div (dom/props {:id "click-outside"}) (dom/text "w/e"))
                             (p/server
                               (let [!v (atom :alice)]
                                 (ui/select (p/watch !v)
                                   (p/fn [v] (reset! !v v))
                                   (p/fn [] (q))
                                   (p/fn [id] (-> data id :name))
                                   #_for-test (reset! !select dom1/node)))))
                           (catch Pending _)
                           (catch Cancelled _)
                           (catch :default e (prn e)))))

     (def select @!select)
     (def input (get-input select))
     (some? input) := true

     "input has the correct value"
     (.-value input) := "Alice B"

     "when we click outside of an open select it closes and reverts value"
     (uit/focus input)
     (uit/click (dom1/by-id "click-outside"))
     (.-value input) := "Alice B"

     (discard)
     ))
