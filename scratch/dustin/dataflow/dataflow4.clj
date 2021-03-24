(ns dustin.dataflow4
  "2021 Jan 31"
  (:require
    [dustin.fiddle :refer [submissions submission]]
    [missionary.core :as m]))


(declare via)


; real world use case
; backend query, reactive
; frontend table render
; function composition

(comment

  '{submissions
    (inc ~>x)

    query-page
    [(submissions (<- >first))

     (let [x (<- >first)]
       (f (postwalk (fn [a]
                      (+ a x #_(<- >first)))
            (range 3))))

     (if (<- open)
       ((fn [e]
          (leolang (submission-detail e (<- open)))) "tempid")
       (submission-detail "tempid"))]
    }

  )

; Proposal to ignore closures for a week - lets make tests pass


(defn ^:client render-table []
  )

(comment

  (def !first (atom "")) (def >first (m/watch !first))
  (def !open (atom false)) (def >open (m/watch !open))

  ; no closures means distinction between
  ; passed argument and lexical argument
  ; is gone, there are no lexical arguments
  ; meaning no reactive special forms inside closures

  ;submissions
  ;(fn2 [>x] (inc (<- >x)))

  ; >first and >open are inputs

  (def ast
    '{popover (fn2 [>open]
                (if (<- >open)
                  (submission "alice")
                  #_(popover >open)                         ; !
                  nil))

      query-page
              (fn2 [>a]
                (render-react!
                  [:html
                   (render-table (submissions (<- >first)))
                   (render-popover (popover >open))]))
      })

  (def ^:dynamic *reactor)
  (def >result (dataflow
                *reactor
                 ast                                        ; program
                 '#{render-table render-popover}            ; passives
                 '#{}
                 '(query-page >first >open)))               ; eagerly run server

  ; ??

  (def !trace (log! reactor))
  @!trace := [{(sm '(<- >first)) ""
               (sm '(submissions (<- >first))) '(9 10 11)
               (sm '>open)  false
               (sm '>popover) nil
               22           ['(9 10 11) nil]                ;(sm '(vector (submissions (<- >first)) (popover >open)))
               '%           ['(9 10 11) nil]}]


  (def client-reactor (dataflow
                        *client-reactor
                        ast
                        '#{submissions popover}
                        '#{render-react!}                          ; which fns are effects?
                        [28 31 22]
                        '(query-page >first >open)          ; same entrypoint
                        ))

  (replay! client-reactor )

  )

; can we assume passives only happen at clojure interop point?
; what does loop recur mean in reactive context?


(comment
  "on deck - cardinality many ast client/server"
  #?(:clj (defn hf-nav [kf e] ...))
  (defn render-record [visit-header visit-row >as]
    (visit-header ~>as)
    (rfor :db/id [>a ~>as]
      (visit-row {:dustingetz/email (hf-nav :dustingetz/email ~>a)
                  :db/id            (hf-nav :dustingetz/email ~>a)})))
  (defn render-edn [>as]
    (render-record
      (fn [as] {'(submissions >needle) as})
      (fn [a] a)
      >as))
  (defn render-table [>as]
    (render-record
      (fn [as] [:table [:span (count as)]])
      (fn [a] [:tr (:dustingetz/email a)])
      >as))


  (def !needle (atom ""))                                   ; from dom <input>
  (def >db ...)                                             ; stream of db vals
  (via
    (let [xs (datomic.api/q '[:find (pull ?e [::email :db/id])
                              :where [?e ::email ?needle]]
               ~>db ~(m/watch !needle))
          el (js/document (.getElementById "#root") (.-innerHTML))]
      (set! el (str "pre" (pr-str xs)))))

  )

