(ns hyperfiddle.router
  (:require [missionary.core :as m]
            [hyperfiddle.api :as hf]
            [contrib.ednish]))

(defmacro path [!path] `(new (m/relieve {} (hyperfiddle.router/path> ~!path))))

(defmacro html5-router [Current-route & body]
  `(let [current-route# ~Current-route]
     (hf/router
       (p/fn [!path#] (new current-route# (contrib.ednish/decode-path (path !path#) hf/read-edn-str)))
       hyperfiddle.router/html5-navigate!
       #(.back js/window.history)
       hyperfiddle.router/html5-replace-state!
       ~@body)))
