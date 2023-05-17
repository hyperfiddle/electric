(ns hyperfiddle.electric-ui4-typeahead-test
  (:require
   #?(:cljs [contrib.dom-test-helpers :as uit])
   [contrib.cljs-target :refer [do-browser]]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.rcf :as rcf :refer [% tests with tap]]
   [hyperfiddle.electric :as p]
   [hyperfiddle.electric :as e]
   [clojure.string :as str]
   [missionary.core :as m]
   [hyperfiddle.electric-dom2 :as dom])
  (:import
   [missionary Cancelled]
   [hyperfiddle.electric Pending]))

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
          (loop [attempt 1]
            (let [nodes (if selector (vec (.querySelectorAll node selector)) node)]
              (when-not (try (pred (transformer nodes)) (catch :default _))
                (when (= 5 attempt)
                  (let [v (try [:ok (transformer nodes)] (catch :default e [:err e]))]
                    (throw (ex-info "failed to assert on selection" {:value v, :pred pred}))))
                (m/? (m/sleep (* 10 attempt attempt))) (recur (inc attempt))))))))))

#?(:cljs
   (defn holds
     ([selection pred] (holds selection identity pred))
     ([selection transformer pred] (selection transformer pred))))

#?(:cljs (defn get-input [tphd] (-> tphd (.getElementsByTagName "input") first)))
#?(:cljs (def options-selector "ul > li"))
#?(:cljs (defn get-options [tphd] (vec (.querySelectorAll tphd options-selector))))
#?(:cljs (defn get-option [tphd s] (some #(when (= s (.-innerText %)) %) (get-options tphd))))

#?(:cljs
   (do-browser
     (tests "basic behavior"
       (def !tphd (atom :missing))
       (def discard (p/run (try
                             (binding [dom/node js/document.body]
                               (p/server
                                 (let [!v (atom :alice)]
                                   (ui/typeahead (p/watch !v)
                                     (p/fn [v] (p/client (tap [:V! v])) (reset! !v v))
                                     (p/fn [search] (p/client (tap [:Options search])) (q search))
                                     (p/fn [id] (p/client (tap [:OptionLabel id])) (-> data id :name))
                                     #_for-test (reset! !tphd dom/node)))))
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
       (tap ::done), % := ::done
       )))


#?(:cljs
   (do-browser
     (tests "controlled value"
       (def !tphd (atom :missing))
       (def !v (atom :alice))
       (def discard (p/run (try
                             (binding [dom/node js/document.body]
                               (let [v (p/watch !v)]
                                 (tap [:controlled-value v])
                                 (p/server
                                   (ui/typeahead v
                                     (p/fn [v] (reset! !v v))
                                     (p/fn [search] (q search))
                                     (p/fn [id] (tap [:OptionLabel id]) (-> data id :name))
                                     #_for-test (reset! !tphd dom/node)))))
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
                             (binding [dom/node js/document.body]
                               (p/server
                                 (let [!v (atom :alice)]
                                   (ui/typeahead (p/watch !v)
                                     (p/fn [v] (reset! !v v))
                                     (p/fn [search] (q search))
                                     (p/fn [id] (-> data id :name))
                                     #_for-test (reset! !tphd dom/node)))))
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
       ((m/sp
          (try
            (m/? (-> (->selection tphd options-selector) (holds count zero?)))
            (m/? (-> (->selection input) (holds #(.-value %) #{"Alice B"})))
            (finally (discard))))
        identity prn))))

#?(:cljs
   (do-browser
     (tests "keyboard"
       (def !tphd (atom :missing))
       (def discard (p/run (try
                             (binding [dom/node js/document.body]
                               (p/server
                                 (let [!v (atom :alice)]
                                   (ui/typeahead (p/watch !v)
                                     (p/fn [v] (reset! !v v))
                                     (p/fn [search] (q search))
                                     (p/fn [id] (-> data id :name))
                                     #_for-test (reset! !tphd dom/node)))))
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

#?(:cljs
   (do-browser
     (tests "truncated result"
       (def !tphd (atom :missing))
       (def discard (p/run (try
                             (binding [dom/node js/document.body]
                               (p/server
                                 (let [!v (atom :alice)]
                                   (ui/typeahead (p/watch !v)
                                     (p/fn [v] (reset! !v v))
                                     (p/fn [search] (take 30 (cycle (q search))))
                                     (p/fn [id] (-> data id :name))
                                     #_for-test (reset! !tphd dom/node)))))
                             (catch Pending _)
                             (catch Cancelled _)
                             (catch :default e (prn e)))))

       (def tphd @!tphd)
       (def input (get-input tphd))
       (some? input) := true

       "when there's too many options we see a signifier telling we should refine our query"
       (uit/focus input)
       ((m/sp
          (try (m/? (-> (->selection tphd "ul > div") (holds #(some-> % first .-innerText) #{"refine your query..."})))
               (finally (discard))))
        identity prn))))
