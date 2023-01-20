(ns hyperfiddle.photon-ui4-typeahead-test
  #?(:cljs (:require-macros hyperfiddle.photon-ui4-typeahead-test))
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
(defn q [search] (into [] (keep (fn [[k {nm :name}]] (when (str/includes? nm search) k))) data))

(tests
  (q "B") := [:alice :bob]
  (q "X") := [])

#?(:cljs (defn get-input [tphd] (-> tphd (.getElementsByTagName "input") first)))
#?(:cljs (defn get-option [tphd s]
           (reduce #(when (= s (.-innerText %2)) (reduced %2)) nil (.querySelectorAll tphd "ul > li"))))

#?(:cljs
   (tests "basic behavior"
     (def !tphd (atom :missing))
     (def discard (p/run (try
                           (binding [dom1/node (dom1/by-id "root")]
                             (p/server
                               (let [!v (atom :alice)]
                                 (ui/typeahead (p/watch !v)
                                   (p/fn [v] (p/client (tap [:V! v])) (reset! !v v))
                                   (p/fn [search] (p/client (tap [:Options search])) (q search))
                                   (p/fn [id] (p/client (tap [:OptionLabel id])) (-> data id :name))
                                   #_for-test (reset! !tphd dom1/node)))))
                           (catch Pending _)
                           (catch Cancelled _)
                           (catch :default e (prn e)))))

     "initially OptionLabel runs on the controlled value"
     % := [:OptionLabel :alice]

     (def tphd @!tphd)
     (def input (get-input tphd))
     (some? input) := true

     "input has the correct value"
     (.-value input) := "Alice B"

     "when focused Options runs along with OptionLabel for each result"
     (uit/focus input)
     % := [:Options "Alice B"]
     % := [:OptionLabel :alice]

     "when we clear the input we'll see a new Options query and the rest of the labels"
     (uit/set-value! input "")
     % := [:Options ""]
     (hash-set % % %) := #{[:OptionLabel :bob] [:OptionLabel :charlie] [:OptionLabel :derek]}

     "when we click on an option V! runs, OptionLabel recalculates the new string and puts it into the input"
     (uit/click (get-option tphd "Charlie D"))
     % := [:V! :charlie]
     % := [:OptionLabel :charlie]
     (.-value input) := "Charlie D"

     (discard)
     ))


#?(:cljs
   (tests "controlled value"
     (def !tphd (atom :missing))
     (def !v (atom :alice))
     (def discard (p/run (try
                           (binding [dom1/node (dom1/by-id "root")]
                             (let [v (p/watch !v)]
                               (tap [:controlled-value v])
                               (p/server
                                 (ui/typeahead v
                                   (p/fn [v] (reset! !v v))
                                   (p/fn [search] (q search))
                                   (p/fn [id] (tap [:OptionLabel id]) (-> data id :name))
                                   #_for-test (reset! !tphd dom1/node)))))
                           (catch Pending _)
                           (catch Cancelled _)
                           (catch :default e (prn e)))))

     "initially OptionLabel runs on the controlled value"
     % := [:controlled-value :alice]
     % := [:OptionLabel :alice]

     (def tphd @!tphd)
     (def input (get-input tphd))
     (some? input) := true

     "input has the correct value"
     (.-value input) := "Alice B"

     "when picking OptionLabel runs for each result"
     (uit/focus input)
     % := [:OptionLabel :alice]

     "while picking controlled value changes have no effect"
     (reset! !v :bob)
     % := [:controlled-value :bob]      ; no OptionLabel call here

     "after picking we see new value flow through"
     (uit/set-value! input "Charlie D")
     % := [:OptionLabel :charlie]
     (uit/click (get-option tphd "Charlie D"))
     % := [:controlled-value :charlie]
     % := [:OptionLabel :charlie]

     (discard)
     ))

#?(:cljs
   (tests "close when clicked outside"
     (def !tphd (atom :missing))
     (def discard (p/run (try
                           (binding [dom1/node (dom1/by-id "root")]
                             (p/server
                               (let [!v (atom :alice)]
                                 (ui/typeahead (p/watch !v)
                                   (p/fn [v] (reset! !v v))
                                   (p/fn [search] (q search))
                                   (p/fn [id] (-> data id :name))
                                   #_for-test (reset! !tphd dom1/node)))))
                           (catch Pending _)
                           (catch Cancelled _)
                           (catch :default e (prn e)))))

     (def tphd @!tphd)
     (def input (get-input tphd))
     (some? input) := true

     "input has the correct value"
     (.-value input) := "Alice B"

     "when we click outside of an open typeahead it closes and reverts value"
     (uit/focus input)
     (uit/set-value! input "")
     (.-value input) := ""
     (uit/click (.querySelector js/document ".hyperfiddle-modal-backdrop"))
     (.-value input) := "Alice B"

     (discard)
     ))
