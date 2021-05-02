(ns hyperfiddle.client.edn-view.links
  (:require ["./links" :refer [linksPlugin]]
            [cljs.reader :as edn]
            [hyperfiddle.api]
            [hyperfiddle.common.ednish :as ednish]))

(def LinksPlugin (linksPlugin edn/read-string ednish/url-encode))
