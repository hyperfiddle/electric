(ns dustin.y2023.pending-frame-nil-bug5
  #?(:cljs (:require-macros dustin.y2023.pending-frame-nil-bug5))
  (:import [hyperfiddle.photon Pending])
  (:require
    [hyperfiddle.photon :as p]
    [hyperfiddle.photon-dom2 :as dom2]))


(p/defn InputController! "callback version, no looped pending" [v V! event]
  (p/with-cycle [?v' nil]
    (let [syncing (= ?v' ::p/pending)]
      (dom2/props {:style {:background-color (if syncing "yellow")}})
      (let [v' (when-some [e (dom2/Event. event syncing)] ; syncing is needed
                 (parse-long (.-target.value ^js e)))]
        (println "input! v' (a): " v') ; works, proving try/catch is the problem
        (try (println "input! v' (b): " v')
             (println "v' is nil? " (nil? v'))
             (when (some? v') (new V! v')) ; move when inside try? No effect
             (catch Pending _ ::p/pending))))))

(defmacro long! [v V! & body]
  `(dom2/input
     (dom2/props {:type "number"})
     ~@body
     (new InputController! ~v ~V! "input")))

(p/defn Demo [] ; both 1. and 2. are needed to reproduce
  (p/server ; 1. wrap in server
    (p/client
      #_(long! 11 (p/fn [v'] (println 'a v')))
      (long! 11 (p/fn [v']
                  (println 'NEVER-NIL v')
                  (p/server (new (p/fn [] v')))))))) ; 2. wrap v' in frame
