(ns hyperfiddle.router
  (:require [missionary.core :as m]))

(defmacro path [!path] `(new (m/relieve {} (hyperfiddle.router/path> ~!path))))