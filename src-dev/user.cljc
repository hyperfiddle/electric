(ns user
  "Photon app server build and run instructions (Clojure and ClojureScript)"
  (:require clojure.string
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.photon :as p]
            ;; Examples
            user.orders-ui
            user.hytradboi
            user.demo-system-properties
            user.demo-server-toggle
            user.photon-live-demo
            user.photon-tutorial-homework))

; Start a REPL with `clj -A:dev`, or jack in with :dev alias.

(comment
  "Start shadow-cljs server"
  (require 'shadow.cljs.devtools.server)
  (shadow.cljs.devtools.server/start!)

  "Compile devkit to javascript"
  ; Watch works. Make sure to eval on JVM first, then save file to trigger cljs compile and reload
  (require '[shadow.cljs.devtools.api :as shadow])
  (shadow/watch :devkit)                                       ; depends on shadow server
  #_(shadow/compile :devkit)
  #_(shadow/release :devkit)

  "Start Photon app server"
  (defonce server (p/start-websocket-server! {:host "localhost" :port 8081}))
  #_(.stop server)

  "Optional CLJS REPL"
  ; shadow server exports an repl
  ; connect a secondary repl instance to this (DO NOT REUSE JVM REPL it will fail weirdly)
  ;   check repl type: eval (type 1)
  (shadow.cljs.devtools.api/repl :devkit))

#?(:cljs (def main user.demo-system-properties/main)) ; Client entrypoint, change to switch example

#?(:cljs (def reactor))                      ; Client process, save for debugging

(defn success [value] #?(:cljs (js/console.log "Reactor success:" value)))
(defn failure [err] #?(:cljs (js/console.error "Reactor failure:" err)))

;; Start and stop reactor on hot code reload
(defn ^:dev/after-load ^:export start! [] #?(:cljs (set! reactor (main success failure))))
(defn ^:dev/before-load stop! [] #?(:cljs (do (when reactor (reactor) #_"teardown") (set! reactor nil))))

(defn includes-str? "used repeatedly in tutorials" [v needle]
  (clojure.string/includes? (clojure.string/lower-case (str v))
                            (clojure.string/lower-case (str needle))))

(tests
  (includes-str? "alice" "e") := true
  (includes-str? "alice" "f") := false
  (includes-str? "alice" "") := true
  (includes-str? "alice" nil) := true
  (includes-str? nil nil) := true
  (includes-str? nil "") := true
  (includes-str? "" nil) := true
  )