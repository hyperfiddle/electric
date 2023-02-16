(ns user.electric.electric-2-transfer
  "Electric with client/server transfer at the REPL"
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.rcf :refer [tests tap %]])
  (:import (hyperfiddle.electric Pending)))

(hyperfiddle.rcf/enable!)

(e/defn App [x]
  (e/client
    (if (even? x)
      (pr-str (type 1))
      (e/server (pr-str (type 1)))))) ; client/server transfer

#?(:cljs
   (tests
     "client/server transfer, pure functional!"
     (def !x (atom 0))
     (def dispose ((e/boot (tap (new App (e/watch !x))))
                   js/console.log js/console.error))
     % := "#object[Number]"
     (swap! !x inc)
     % := "java.lang.Long"                  ; holy cow
     (dispose)))

(comment
  ; connect a new NREPL do not use existing JVM repl !!!
  ; do not eval in your existing JVM repl it wont work
  (shadow.cljs.devtools.api/repl :dev)
  ; Connect browser session - http://localhost:8080
  ; Browser console: shadow-cljs: #3 ready!
  (type 1)
  (println 1)  ; see browser console
  (tests (pr-str (type 1)) := "#object[Number]")  ; see ✅ in browser console
  )
