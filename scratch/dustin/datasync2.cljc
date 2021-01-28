(ns dustin.datasync2
  (:require
    [hyperfiddle.api :as hf]
    [hyperfiddle.incremental :refer [fmapI capI bindI]]
    [minitest :refer [tests]]
    [missionary.core :as m]
    [datascript.core :as d]
    [dustin.fiddle :as f]
    [dustin.compiler3 :refer [dataflow dataflow* log! source-map replay!]]))

(declare something-else)

; we want to make two reactors communicate over wire
; communicate info about what happens at arbitrary points of ast

(defn render [xs]
  [:table
   [:tr xs]
   #_(rfor [x :db/id xs]
       [:tr (pr-str x)])])

(tests
  (render []) := [:table [:tr []]])

(defn submissions [$ needle]
  (binding [hf/*$* $]
    (f/submissions needle)))

(defn query-route [>$ [f & args :as route]]
  (case f

    dustin.fiddle/submissions
    (let [[needle] args]
      (fmapI #(submissions % needle) >$))

    (m/watch (atom 404))))

(defn router [>$ >route]
  (fmapI render (bindI >route #(query-route >$ %))))

;;;;;;;;;;;;;
;; GENERIC ;;
;;;;;;;;;;;;;

(tests
 (def ast '(fmap render (bind >route qr)))
 (def sm (source-map ast))
 (first (sm '>route)) := 2
 (first (sm '(bind >route qr))) := 1
 )

(tests
  (def !route (atom nil))
  (def !$ (atom hf/*$*))
  (def >route (m/watch !route))
  (def >$ (m/watch !$))
  (def qr (partial query-route >$))

  (def d (dataflow (fmap render (bind >route qr))))
  (def !trace (log! d))

  (reset! !route ['dustin.fiddle/submissions "alice"])
  (submissions hf/*$* "alice") := '(9)

  @!trace :=
  [{[2] ['dustin.fiddle/submissions "alice"]
    [1] '(9)
    [0] [:table [:tr '(9)]]}]
  )

;;;;;;;;;;;;
;; CLIENT ;;
;;;;;;;;;;;;

(tests
 (def !route (atom nil))
 (def !$ (atom hf/*$*))
 (def >route (m/watch !route))
 (def >$ (m/watch !$))
 (def qr (partial query-route >$))

 (def d (dataflow (fmap render (bind >route qr))
                  #{1} ;; bind
                  ))
 (def !trace (log! d))


 (reset! !route ['dustin.fiddle/submissions "alice"]) ;; set by client
 (replay! d {[1] '(9)})

 @!trace :=
 [{[2] ['dustin.fiddle/submissions "alice"]}
  ;; [1] is passive so it isn't traced
  {[0] [:table [:tr '(9)]]}]
 )

;;;;;;;;;;;;
;; SERVER ;;
;;;;;;;;;;;;

(tests
 ;; (def !route (atom nil))
 ;; (def >route (m/watch !route))
 (def !$ (atom hf/*$*))
 (def >$ (m/watch !$))
 (def qr (partial query-route >$))

 (def d (dataflow (fmap render (bind >route qr))
                  #{0 2} ;; (fmap render …) and >route passive
                  ))
 (def !trace (log! d))


 (reset! !route ['dustin.fiddle/submissions "alice"]) ;; set by client
 (replay! d {[2] ['dustin.fiddle/submissions "alice"]})

 @!trace :=
 ;; [2] is passive, so not traced. [0] is client-side only (passive as well) It
 ;; should not be part of the server ast. ASTs should compose.
 [{[1] '(9)}]

 )
