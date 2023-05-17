(ns hyperfiddle.electric-dom2-test
  (:require
   [contrib.cljs-target :refer [do-browser]]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.rcf :as rcf :refer [% tap tests with]]
   [missionary.core :as m]
   #?(:cljs [contrib.dom-test-helpers :as uit]))
  (:import
   #?(:clj [clojure.lang ExceptionInfo])
   [hyperfiddle.electric Pending]
   [missionary Cancelled]))

#?(:cljs
   (do-browser
     (tests "dom/focused?, dom/hovered?"
       (def !in (atom nil))
       (with (e/run (binding [dom/node (.-body js/document)]
                      (dom/input
                        (reset! !in dom/node)
                        (tap [(dom/Focused?.) (dom/Hovered?.)]))))
         #_init                     % := [false false]
         (uit/focus @!in)           % := [true  false]
         (uit/hover @!in)           % := [true  true]))))

#?(:cljs
   (do-browser
     (tests "e/listen>"
       (def !in (atom nil))
       (def !it (atom nil))
       (with (e/run (binding [dom/node (.-body js/document)]
                      (dom/input
                        (reset! !in dom/node)
                        (reset! !it (e/listen> dom/node "keydown"
                                      (fn [e] (when (= (.-key e) "Enter") (.-value dom/node))))))))
         (def it (@!it #() #()))
         (uit/set-value! @!in "hi") (uit/press @!in "Enter") @it := "hi"
         (uit/set-value! @!in "ho") (uit/press @!in "Enter") @it := "ho"
         (it)))))

#?(:cljs
   (do-browser
     (tests "dom text node mount and unmount"
       (def !div (atom nil))
       (def !mounted? (atom true))
       (with (e/run (binding [dom/node (.-body js/document)]
                      (dom/div
                        (reset! !div dom/node)
                        (when (e/watch !mounted?) (dom/text "hello") :mounted))))
         (.-textContent @!div) := "hello"
         (swap! !mounted? not)
         (.-textContent @!div) := ""))

     (tests "switch on dom effect"
       (def !div (atom nil))
       (def !x (atom true))
       (with (e/run (binding [dom/node (.-body js/document)]
                      (dom/div
                        (reset! !div dom/node)
                        (dom/text "a")
                        (dom/text (if (e/watch !x) "b" "c"))
                        (dom/text "d"))))
         (.-textContent @!div) := "abd"
         (swap! !x not)
         (.-textContent @!div) := "acd"
         (swap! !x not)
         (.-textContent @!div) := "abd"))

     (tests "dynamic dom ordering"
       (def !div (atom nil))
       (def !xs (atom ["<" ">"]))
       (with (e/run (binding [dom/node (.-body js/document)]
                      (dom/div
                        (reset! !div dom/node)
                        (dom/text ".")
                        (e/for [x (e/watch !xs)]
                          (dom/text x))
                        (dom/text "."))))
         (.-textContent @!div) := ".<>."
         (swap! !xs reverse)
         (.-textContent @!div) := ".><."
         (swap! !xs reverse)
         (.-textContent @!div) := ".<>."))

     (tests "namespaced attributes"
       (dom/resolve-attr-alias :href) := [nil "href"]
       (dom/resolve-attr-alias "href") := [nil "href"]
       (dom/resolve-attr-alias :svg:rect) := ["http://www.w3.org/2000/svg" "rect"]
       (dom/resolve-attr-alias :xlink:href) := ["http://www.w3.org/1999/xlink" "href"]
       (dom/resolve-attr-alias "xlink:href") := ["http://www.w3.org/1999/xlink" "href"]))
   )
