(ns hyperfiddle.client.edn-view.links
  (:require ["./links" :refer [linksPlugin]]
            [cljs.reader :as edn]
            [hyperfiddle.api]))

(defn- parse-link [s]
  (edn/read-string s))

(def LinksPlugin (linksPlugin parse-link))
