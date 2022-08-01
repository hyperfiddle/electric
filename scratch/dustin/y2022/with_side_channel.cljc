(ns dustin.y2022.with-side-channel)


#?(:clj
   (defmacro try-catch-non-fatal [& args]
     (let [[try-body [e catch-body]] (split-at (- (count args) 2) args)]
       `(try
          ~@try-body
          ~(if (:ns &env)
             `(catch js/Error ~e ~catch-body)
             `(catch Throwable ~e
                (if (#{VirtualMachineError
                       ThreadDeath
                       InterruptedException
                       LinkageError} (type ~e))
                  (throw ~e)
                  ~catch-body)))))))

#?(:clj
   (defmacro try& [& body]
     (let [e (gensym)]
       `(try-catch-non-fatal
          [nil (do ~@body)]
          ~e [~e nil]))))

(defn empty-str? [?s] (when (string? ?s) (zero? (count ?s))))
(defn empty->nil [?s] (if (empty-str? ?s) nil ?s))

#?(:clj
   (defmacro with-io
     "like with-out-str but returns [err out-str result] and adds more bindings"
     [& body]
     `(let [s# (new java.io.StringWriter)]
        (binding [*out* s#] #_ ~@bindings
          (let [[err# result#] (try& ~@body)]
            [err# (empty->nil (str s#)) result#])))))

(comment
  (with-io (println 'yo)) := [nil "yo\n" nil]
  (with-io (do (println 'yo) 42)) := [nil "yo\n" 42]
  (with-io (do (assert false "a") 42)) := [?e nil nil]
  )

(def ^:dynamic *side-channel*)
(defmacro with-side-channel
  "lets exceptions through"
  [& body]
  `(binding [*side-channel* nil]
     (let [x#    (do ~@body)                                ; opportunity to (set! *side-channel ...)
           side# *side-channel*]                            ; capture any mutation
       (set! *side-channel* nil)                            ; prevent forward leak, not sure if needed
       [side# x#])))

(comment
  "with-side-channel example usage"
  ; superadmin-edit-request-pure calls check-offsets-pure, which uses a side-channel to tunnel an out-of-band result
  ; from a nested function, in a way that is invisible to intermediate call stack layers. What is achieved is
  ; the intermediate callers do not need to be lifted and aware of the tupled result type, with the added benefit of
  ; retaining backwards compat with the effectful interface.
  (let [[action tx] (with-side-channel
                      (suber2.web.sub-req/superadmin-edit-request-pure
                        (:db/id user) (:db/id sub-req) edit-map))]
    (when-let [[f & args] action]
      (case f `suber.web.sentry/send-event (apply suber.web.sentry/send-event args)))
    (d/transact conn tx)))
