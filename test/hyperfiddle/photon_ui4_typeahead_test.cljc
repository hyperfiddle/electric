(ns hyperfiddle.photon-ui4-typeahead-test
  #?(:cljs (:require-macros hyperfiddle.photon-ui4-typeahead-test))
  (:require
   #?(:cljs [hyperfiddle.ui.test :as uit])
   [contrib.cljs-target :refer [do-browser]]
   [hyperfiddle.photon-dom :as dom1]
   [hyperfiddle.photon-ui4 :as ui]
   [hyperfiddle.rcf :as rcf :refer [% tests with tap]]
   [hyperfiddle.photon :as p]
   [clojure.string :as str]
   [missionary.core :as m]
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

#?(:cljs
   (defn ->selection
     ([node] (->selection node nil))
     ([node selector]
      (fn [transformer pred]
        (m/sp
          (loop [try 1]
            (let [nodes (if selector (vec (.querySelectorAll node selector)) node)]
              (when-not (pred (transformer nodes))
                (when (= 5 try)
                  (throw (ex-info "failed to assert on selection" {:value (transformer nodes), :pred pred})))
                (m/? (m/sleep (* 10 try try))) (recur (inc try))))))))))

#?(:cljs
   (defn holds
     ([selection pred] (holds selection identity pred))
     ([selection transformer pred] ((selection transformer pred) identity prn))))

#?(:cljs (defn get-input [tphd] (-> tphd (.getElementsByTagName "input") first)))
#?(:cljs (def options-selector "ul > li"))
#?(:cljs (defn get-options [tphd] (vec (.querySelectorAll tphd options-selector))))
#?(:cljs (defn get-option [tphd s] (some #(when (= s (.-innerText %)) %) (get-options tphd))))

#?(:cljs
   (do-browser
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
       % := [:Options ""]
       (hash-set % % % %) := #{[:OptionLabel :alice] [:OptionLabel :bob] [:OptionLabel :charlie] [:OptionLabel :derek]}

       "when we click on an option V! runs, OptionLabel recalculates the new string and puts it into the input"
       (uit/click (get-option tphd "Charlie D"))
       % := [:V! :charlie]
       % := [:OptionLabel :charlie]
       (.-value input) := "Charlie D"

       (discard)
       )))


#?(:cljs
   (do-browser
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
       (hash-set % % % %) := #{[:OptionLabel :alice] [:OptionLabel :bob] [:OptionLabel :charlie] [:OptionLabel :derek]}

       "while picking controlled value changes have no effect"
       (reset! !v :bob)
       % := [:controlled-value :bob]    ; no OptionLabel call here

       "after picking we see new value flow through"
       (uit/click (get-option tphd "Charlie D"))
       % := [:controlled-value :charlie]
       % := [:OptionLabel :charlie]

       (discard)
       )))

#?(:cljs
   (do-browser
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
       (-> (->selection tphd options-selector) (holds count zero?))
       (-> (->selection input) (holds #(.-value %) #{"Alice B"}))

       (discard)
       )))

#?(:cljs
   (do-browser
     (tests "keyboard"
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

       "escape closes typeahead"
       (uit/focus input)
       (count (get-options tphd)) := (count data)
       (uit/press (get-option tphd "Bob C") "Escape")
       (count (get-options tphd)) := 0

       "down arrow & Enter selects Bob"
       (uit/focus input)
       (count (get-options tphd)) := (count data)
       (uit/press tphd "ArrowDown")
       (uit/press tphd "Enter")
       (count (get-options tphd)) := 0
       (.-value input) := "Bob C"

       "Enter on bad input is noop"
       (def original-value (.-value input))
       (uit/focus input)
       (count (get-options tphd)) := (count data)
       (uit/set-value! input "xyz")
       (count (get-options tphd)) := 0
       (uit/press tphd "Enter")
       (.-value input) := "xyz"         ; noop
       (uit/press tphd "Escape")        ; reverts
       (.-value input) := original-value

       "typing into input filters correctly"
       (uit/focus input)
       (uit/set-value! input "B")
       (into #{} (map #(.-innerText %)) (get-options tphd)) := #{"Alice B" "Bob C"}
       (uit/press tphd "Escape")

       (discard)
       )))
