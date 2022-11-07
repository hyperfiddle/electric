(ns dustin.scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.scratch)))

; (binding [!stage (atom [])]
;  (let [stage (p/watch !stage)]
;    ...))

(defmacro with-state [[anaphor x] & body]
  (let [a '~anaphor
        !a (symbol (str "!" '~anaphor))]
    `(binding [~!a (atom ~x)]
       (let [~a (p/watch ~!a)]
         ~@body))))

(p/def stage)
(def ^:dynamic !stage)

(with-state [stage 1]
  (dom/pre (pr-str stage))
  (ui/button {::ui/click-event (p/fn [e] (swap! stage inc))} "inc"))


(comment

  (p/defn CheckBox [class checked Change!]
    (p/with-state false
                  (dom/span {:class (if p/state "user-busy")}
                    (dom/input {:type "checkbox"
                                :class class
                                :checked checked
                                :aria-busy p/state
                                :on-input (p/fn [_] (Change! (.-checked dom/node)))}))))

  )