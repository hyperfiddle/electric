(ns hyperfiddle.electric.shadow-cljs.hooks-de
  (:require [clojure.string :as str]
            [hyperfiddle.electric.impl.lang-de2 :as lang]
            [hyperfiddle.electric.impl.cljs-analyzer2 :as cljs-ana]))

(let [!first-run? (volatile! true)]     ; first run is noop
  (defn reload-clj
    "When any Electric def is changed, recompile it in both Clojure and ClojureScript
(because the expression may contain e/client and/or e/server). Takes care to prevent
double reloads (i.e. from :require-macros)."
    {:shadow.build/stage :compile-finish} [build-state]
    (prn ::reload-hook)
    (if @!first-run?
      (vreset! !first-run? false)
      (when (= :dev (:shadow.build/mode build-state))
        (let [compiled-keys (-> build-state :shadow.build/build-info :compiled)
              cljc-infos (eduction (filter (fn [[_ f]] (str/ends-with? f ".cljc")))
                           (map #(get (:sources build-state) %)) compiled-keys)]
          (doseq [{ns-sym :ns, macro-requires :macro-requires} cljc-infos]
            (when (and (not (get macro-requires ns-sym)) (-> ns-sym find-ns meta ::lang/has-edef?))
              (prn ::reloading ns-sym)
              (swap! lang/!a cljs-ana/purge-ns ns-sym)
              (require ns-sym :reload))))))
    build-state))
