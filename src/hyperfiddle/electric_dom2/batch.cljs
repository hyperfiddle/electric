(ns hyperfiddle.electric-dom2.batch)

(def queue #js[]) ; a queue

(defn schedule!
  ([f] (schedule! nil f))
  ([flag f]
   #_(when flag
     (prn "pushed " flag ))
   (.push queue f)
   (when (= 1 (.-length queue))
     (js/requestAnimationFrame
       (fn []
         ;; (prn "batch start" (.-length queue))
         (.forEach queue (fn [f]
                           (try (f)
                                (catch :default e
                                  (js/console.error `schedule! "Exception while running a batched dom effect" e)))))
         ;; (prn "batch end")
         (set! queue #js []))))))
