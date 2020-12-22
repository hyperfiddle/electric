(ns dustin.lens
  (:refer-clojure :exclude [nth key keys vals filter select-keys cat derive])
  (:require
    [lentes.core :refer :all]
    [minitest :refer [tests]]))

; https://cljdoc.org/d/funcool/lentes/1.3.3/doc/user-guide

(tests
  (defn get-foo [state] (:foo state))

  (defn set-foo [state f] (update state :foo f))

  (def data {:foo 1 :bar 2})

  (get-foo data)
  := 1

  (set-foo data inc)
  := {:foo 2 :bar 2}

  (def foo-lens (lens get-foo set-foo))
  (focus foo-lens data)
  := 1

  (over foo-lens inc data)
  := {:foo 2 :bar 2}

  (def mylens (key :foo))
  (focus mylens data)
  := 1

  (focus (key :foo) data)
  := 1

  (def l (lens
           (fn [s] (get s :foo))
           (fn [s f] (update s :foo f))))

  (focus l data)
  := 1

  ; (focus :foo data)
  ; NPE


  ; Atoms
  (def state1 (atom {:foo 1 :bar 1}))
  (def state2 (derive (key :foo) state1))

  @state1
  @state2

  (swap! state2 inc)
  (swap! state1 update :foo inc)

  ; composition


  (def ffst (comp l/fst l/fst))

  )
