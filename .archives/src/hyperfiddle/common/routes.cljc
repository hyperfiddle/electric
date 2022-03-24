(ns hyperfiddle.common.routes
  (:require [missionary.core :as m]))

(defonce !route (atom nil))
(def >route #?(:cljs (m/watch !route)))

(def ROUTES ["/" [#_["/auth" ::auth]
                  [[:sexpr ".*"] ::sexpr]]])
