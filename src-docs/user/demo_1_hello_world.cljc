(ns user.demo-1-hello-world
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

; First demonstration of client/server transfer:
; a full stack function with both frontend and backend parts,
; all defined in the same expression

(e/defn HelloWorld []
  (e/client
    (dom/h1 (dom/text "Hello Electric"))
    (dom/div (dom/text "Hello from server, where JVM number type is: ")
      (dom/code (dom/text (e/server (pr-str (type 1))))))
    (dom/div (dom/text "Hello from client, where JS number type is: ")
      (dom/code (dom/text (e/client (pr-str (type 1))))))))
