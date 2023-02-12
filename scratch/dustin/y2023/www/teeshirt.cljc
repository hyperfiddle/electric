(ns dustin.y2023.www.teeshirt
  #?(:cljs (:require-macros dustin.y2023.www.teeshirt))
  (:import [hyperfiddle.electric Pending])
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]))

(declare query-teeshirt-orders)

(e/defn Teeshirt-orders [db]
  (e/client
    (dom/h1 (dom/text "Tee-shirt orders"))
    (let [!email (atom "") email (e/watch !email)]
      (ui/input email (e/fn [v] (reset! !email v)))
      (dom/table
        (try
          (e/server
            (e/for [x (query-teeshirt-orders db email)]
              (e/client (dom/tr (dom/text (pr-str x))))))
          (catch Pending e
            (dom/props {:class "loading"})))))))