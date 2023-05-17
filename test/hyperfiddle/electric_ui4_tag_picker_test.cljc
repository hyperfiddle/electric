(ns hyperfiddle.electric-ui4-tag-picker-test
  #?(:cljs (:require-macros hyperfiddle.electric-ui4-tag-picker-test))
  (:import [missionary Cancelled] [hyperfiddle.electric Pending])
  (:require
   [clojure.string :as str]
   [contrib.cljs-target :refer [do-browser]]
   #?(:cljs [contrib.dom-test-helpers :as uit])
   [hyperfiddle.electric :as p]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.rcf :as rcf :refer [% tests with tap]]))

(def data {:alice   {:name "Alice B"}
           :bob     {:name "Bob C"}
           :charlie {:name "Charlie D"}
           :derek   {:name "Derek E"}})
(defn q [search] (into [] (keep (fn [[k {nm :name}]] (when (str/includes? nm search) k))) data))

(tests
  (q "B") := [:alice :bob]
  (q "X") := [])

#?(:cljs (defn get-input [tgpk] (-> tgpk (.getElementsByTagName "input") first)))
#?(:cljs (defn get-options [tgpk] (vec (.querySelectorAll tgpk ".hyperfiddle-tag-picker-input-container > ul > li"))))
#?(:cljs (defn get-picked-items [tgpk] (vec (.querySelectorAll tgpk ".hyperfiddle-tag-picker-items > li"))))
#?(:cljs (defn find-with-text [elems s] (some #(when (str/includes? (.-innerText %) s) %) elems)))
#?(:cljs (defn children [elem] (vec (.-children elem))))

#?(:cljs
   (do-browser
     (tests "basic behavior"
       (def !tgpk (atom :missing))
       (def discard (p/run (try
                             (binding [dom/node js/document.body]
                               (p/server
                                 (let [!v (atom #{:alice :bob})]
                                   (ui/tag-picker (p/watch !v)
                                     (p/fn [v] (tap [:V! v]) (swap! !v conj v))
                                     (p/fn [v] (tap [:unV! v]) (swap! !v disj v))
                                     (p/fn [search] (tap [:Options search]) (q search))
                                     (p/fn [id] (tap [:OptionLabel id]) (-> data id :name))
                                     #_for-test (reset! !tgpk dom/node)))))
                             (catch Pending _)
                             (catch Cancelled _)
                             (catch :default e (prn e)))))

       "initially OptionLabel runs on the controlled values"
       (hash-set % %) := #{[:OptionLabel :alice] [:OptionLabel :bob]}

       "the picked values are rendered"
       (def tgpk @!tgpk)
       (count (get-picked-items tgpk)) := 2

       "there's an input to pick more, and is empty"
       (def input (get-input tgpk))
       (some? input) := true
       (.-value input) := ""

       "when focused Options runs along with OptionLabel for each result"
       (uit/focus input)
       % := [:Options ""]
       (hash-set % % % %) := #{[:OptionLabel :alice] [:OptionLabel :bob] [:OptionLabel :charlie] [:OptionLabel :derek]}

       "when we click on an option V! runs, OptionLabel recalculates the new string and the input gets cleared"
       (uit/set-value! input "Cha")
       % := [:Options "Cha"]
       (uit/click (-> (get-options tgpk) (find-with-text "Charlie D")))
       ;; order doesn't matter
       (hash-set % %) := #{[:V! :charlie] [:OptionLabel :charlie]}
       (.-value input) := ""

       "clicking an item's × removes it"
       (def picked-count-before (count (get-picked-items tgpk)))
       (def remove-alice (-> (get-picked-items tgpk) (find-with-text "Alice B") children first))
       (uit/click remove-alice)
       % := [:unV! :alice]
       (count (get-picked-items tgpk)) := (dec picked-count-before)

       "clicking an item opens the options"
       (uit/click (-> (get-picked-items tgpk) first))
       % := [:Options ""]
       (hash-set % % % %) := #{[:OptionLabel :alice] [:OptionLabel :bob] [:OptionLabel :charlie] [:OptionLabel :derek]}

       (discard)
       )))

(p/defn No1 [_])

#?(:cljs
   (do-browser
     (tests "nil V! disables input"
       (def !tgpk (atom :missing))
       (with (p/run (try
                      (binding [dom/node js/document.body]
                        (p/server
                          (ui/tag-picker [] nil No1 No1 No1
                            #_for-test (reset! !tgpk dom/node))))
                      (catch Pending _)
                      (catch Cancelled _)
                      (catch :default e (prn e))))

         (def tgpk @!tgpk)
         (def input (get-input tgpk))
         (some? input) := true
         (.-disabled input) := true))))
