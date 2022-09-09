(ns dustin.y2022.css-grid
  (:require [hyperfiddle.rcf :as rcf :refer [tests ! % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m])
  #?(:cljs (:require-macros dustin.y2022.css-grid)))

(hyperfiddle.rcf/enable!)

; https://www.w3schools.com/css/css_grid.asp
; basics https://jsfiddle.net/qxxpgg4j/
; table layout https://codesandbox.io/s/css-grid-blog-final-duwp7?file=/src/styles.scss
; sticky headers https://codepen.io/nchevobbe/pen/bYZEqq?editors=0110
; scroll https://codepen.io/lookininward/pen/zYOQjZM?editors=1100
; fr unit https://css-tricks.com/introduction-fr-css-unit/
; reference guide https://css-tricks.com/snippets/css/complete-guide-grid/
; striping rows css grid https://stackoverflow.com/questions/44936917/table-striping-rows-in-css-grid
