(ns hyperfiddle.router
  (:require [missionary.core :as m]
            [hyperfiddle.api :as hf]
            [contrib.ednish]))

(defmacro path [!path] `(new (m/relieve {} (hyperfiddle.router/path> ~!path))))

