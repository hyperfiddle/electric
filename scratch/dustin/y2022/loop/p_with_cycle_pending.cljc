
(defmacro with-cycle2 [[s i] & body]
  `(let [x# (with-cycle [~s ~i]
                        (try
                          (do ~@body)
                          (catch Pending _# ::pending)))]
     (if (= ::pending x#) (throw (Pending.)) x#)))

(comment (macroexpand '(with-cycle2 [busy true] false)))