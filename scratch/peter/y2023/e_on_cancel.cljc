(ns peter.y2023.e-on-cancel)

(cc/defn -on-cancel [f]
  (cc/fn [n t]
    (n)
    (reify
      IFn (#?(:clj invoke :cljs -invoke) [_] (f) (t))
      IDeref (#?(:clj deref :cljs -deref) [_]))))

(defmacro on-cancel [f] `(new (-on-cancel ~f)))

(p/defn MountCancel [tap x]
  (tap [:mount x])
  (p/on-cancel #(tap [:cancel x])))

(tests "current mount and cancel order (could mount order be defined and cancel order be flipped?)"
  (with (p/run (MountCancel. tap :x) (MountCancel. tap :y)))
  % := [:mount :x]
  % := [:mount :y]
  % := [:cancel :x]
  % := [:cancel :y])

(tests
  (def !x (atom 0))
  (with (p/run
          (case (p/watch !x)
            0 (MountCancel. tap 0)
            1 (MountCancel. tap 1)))
    % := [:mount 0]
    (reset! !x 1)
    % := [:mount 1]
    % := [:cancel 0]))