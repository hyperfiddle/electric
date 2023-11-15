(ns hyperfiddle.electric.shadow-cljs.hooks
  (:require [hyperfiddle.electric :as-alias e]
            [hyperfiddle.electric.impl.lang :as lang]
            [clojure.string :as str]))

(defn reload-clj "If a cljc file with electric defs changed and doesn't require-macros itself, reload it.

  The reload is necessary to keep the server and client portion of an e/def in sync."
  {:shadow.build/stage :compile-finish} [build-state]
  (let [compiled-keys (-> build-state :shadow.build/build-info :compiled)
        cljc-infos (eduction (filter (fn [[_ f]] (str/ends-with? f ".cljc")))
                     (map #(get (:sources build-state) %)) compiled-keys)]
    (doseq [{ns-sym :ns, macro-requires :macro-requires} cljc-infos]
      (when (and (not (get macro-requires ns-sym)) (-> ns-sym find-ns meta ::lang/has-edef?))
        (prn ::reloading ns-sym)
        (require ns-sym :reload))))
  build-state)
