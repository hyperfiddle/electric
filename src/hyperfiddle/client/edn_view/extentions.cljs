(ns hyperfiddle.client.edn-view.extentions
  (:require ["./extentions" :refer [extentionsPlugin WidgetBuilder]]
            [cljs.reader :as edn]
            [hyperfiddle.api :refer [Link Input]]
            [geoffrey.ui-inputs :as ui]
            [hyperfiddle.common.ednish :as ednish]))

(defn parseTaggedValue [str]
  (let [edn (edn/read-string str)]
    (condp = (type edn)
      Link  ["Link" edn]
      Input ["Input" (ui/get-input! edn)])))

(def ExtentionsPlugin (extentionsPlugin (WidgetBuilder. ednish/url-encode parseTaggedValue)))
