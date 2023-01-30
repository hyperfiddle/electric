(ns wip.dennis-exception-leak
  #?(:cljs (:require-macros wip.dennis-exception-leak))
  (:require
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-dom2 :as dom2]
   [hyperfiddle.photon-ui :as ui]))

(p/defn App []
  (p/client
    ;; 1) will not error but once input 2) errored it will rethrow on each input
    (ui/input {::dom/placeholder "1) will not error"
               ::ui/input-event (p/fn [e]
                                  (let [v (.. e -target -value)]
                                    (p/server
                                      (println "input 1:" v))))})
    (ui/input {::dom/placeholder "2_ will error"
               ::ui/input-event (p/fn [e]
                                  (let [v (.. e -target -value)]
                                    (p/server
                                      (println "input 2:" v)
                                      (throw (ex-info "oh no" {:error true})))))})))

(p/defn App2 []
  (p/client
    (dom2/input (dom2/props {:placeholder "1) will not error"})
      (dom2/on "input" (p/fn [e]
                         (let [v (.. e -target -value)]
                           (println "dom2 input 1:" v)))))
    (throw (ex-info "oh no" {:error true}))))
