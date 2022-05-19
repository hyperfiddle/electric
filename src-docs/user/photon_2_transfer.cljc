(ns user.photon-2-transfer
  "Photon with client/server transfer at the REPL"
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.rcf :refer [tests ! % with]])
  (:import (hyperfiddle.photon Pending)))


(hyperfiddle.rcf/enable!)

(p/defn App [x]
  (if (even? x)
    (pr-str (type 1))
    ~@(pr-str (type 1))))                   ; client/server transfer

#?(:cljs
   (tests
     "client/server transfer, pure functional!"
     (def !x (atom 0))
     (def dispose ((p/client (p/main (try (! (App. (p/watch !x)))
                                       (catch Pending _))))
                   js/console.log js/console.error))
     % := "#object[Number]"
     (swap! !x inc)
     % := "java.lang.Long"                  ; holy cow
     (dispose)))

(def main #?(:cljs (fn [s f])))

(comment
  #?(:clj (def dispose (user/browser-main! :main `main)))
  #?(:clj (dispose))

  ; connect a new NREPL do not use existing JVM repl !!!
  ; do not eval in your existing JVM repl it wont work
  (shadow.cljs.devtools.api/repl :app)
  ; Connect browser session - http://localhost:8080
  ; Browser console: shadow-cljs: #3 ready!
  (type 1)
  (println 1)  ; see browser console
  (tests (pr-str (type 1)) := "#object[Number]")  ; see ✅ in browser console
  )
