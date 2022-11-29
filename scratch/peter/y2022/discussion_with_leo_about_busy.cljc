(ns peter.y2022.discussion-with-leo-about-busy)

(comment
  ;; P: code like this
  (p/server (reset! !x (-> (new (m/relieve {} (m/reductions {} nil (m/observe "click")))) .-target .-value)))
  ;; creates a continuous flow that looks like
  ;;
  ;;                e3
  ;;           e2    |--------
  ;;      e1    |----
  ;;       |----
  ;; ------
  ;;
  ;; The busy/impulse pattern creates
  ;;                e3
  ;;           e2    |----|
  ;;      e1    |--| |    |
  ;;       |--| |  | |    |
  ;; ------    -    -      ---
  ;;
  ;; Leo, why do you want to model discrete events through the impulse pattern?
  ;;
  ;; L: what happens if e2 flows in before e1 got fully processed?
  ;; In the first case there will be a switch and the branch handling e1 gets cancelled.
  ;; In the second case it should be possible to define different behavior.
  ;; E.g. if e2 happens while e1 is still being processed e2 could be ignored.
  ;; Another problem is how to compose 2 or more event flows with the first case.
  ;;
  ;; P: I haven't fully understood the cancellation problem yet.
  ;; I thought about the composition pattern and I have an operator `flow-case` in dom_pure.cljc.
  ;;
  ;; Leo took a close look on the operator and said:
  ;; The operator is correct/sound. I think it is too specific
  ;; and won't be able to handle more complex cases, not sure though.
  ;; It would be interesting to see if you can fix the ui/input control bug
  ;; where on blur the latest value can get discarded with `flow-case` if you find the time.
  ;;
  ;; P: Do you have a fix for that? Are you fixing it by introducing a busy flag in the input?
  ;;
  ;; L: Yes I'm introducing a busy flag. I'm working on the fix right now.
  )
