;; Temporary ns, might be merge to dom3 later

(ns hyperfiddle.dom3-events)

#?(:cljs
   (defn listen> [nd typ f opts]
     (m/observe (fn [!]
                  (! nil)
                  (let [! (comp ! f), opts (clj->js opts)]
                    (.addEventListener nd typ ! opts)) #(.removeEventListener nd typ ! opts)))))

(defmacro listen
  ([typ] `(listen ~typ identity))
  ([typ f] `(listen ~typ ~f node))
  ([nd typ f] `(listen ~nd ~typ ~f nil))
  ([nd typ f opts] `(listen> ~nd ~typ ~f ~opts)))

;; ----------

#?(:cljs
   ;; TODO starts as empty incseq, later singleton changing value
   (defn listen1 [nd typ f opts]
     (m/observe (fn [!]
                  (! nil)
                  (let [! (comp ! f) , opts (clj->js opts)]
                    (.addEventListener nd typ ! opts)) #(.removeEventListener nd typ ! opts)))))

(defmacro listen2
  ([typ] `(listen ~typ identity))
  ([typ f] `(listen node ~typ ~f))
  ([nd typ f] `(listen ~nd ~typ ~f nil))
  ([nd typ f opts] `(e/input (listen1 ~nd ~typ ~f ~opts))))

(defn append-only [<xs]
  (->> <xs
    (m/eduction (map-indexed (fn [i x] {:grow 1 :degree (inc i) :shrink 0 :permutation {} :change {i x} :freeze #{i}})))
    (m/reductions {} (i/empty-diff 0))
    (m/relieve i/combine)))

#?(:cljs
   (defn event-log* [nd typ f opts]
     (append-only
       (m/observe (fn [!]
                    (let [! #(when-some [v (f %)] (! v)) , opts (clj->js opts)]
                      (.addEventListener nd typ ! opts)) #(.removeEventListener nd typ ! opts))))))

(defmacro event-log
  ([typ] `(event-log ~typ identity))
  ([typ f] `(event-log node ~typ ~f))
  ([nd typ f] `(event-log ~nd ~typ ~f nil))
  ([nd typ f opts] `(e/join (event-log* ~nd ~typ ~f ~opts))))


;;;;;;;;;;;;;;;;;;;;;;
;;; NEXT ITERATION ;;;
;;;;;;;;;;;;;;;;;;;;;;

#?(:cljs (defn listen*
           ([node typ] (listen* node typ identity))
           ([node typ f] (listen* node typ f {}))
           ([node typ f opts]
            (m/observe (fn [!]
                         (let [! #(! (f %)), opts (clj->js opts)]
                           (.addEventListener node typ ! opts)
                           #(.removeEventListener node typ ! opts)))))))

#?(:cljs (defn listen*-some
           ([node typ] (listen*-some node typ identity))
           ([node typ f] (listen*-some node typ f {}))
           ([node typ f opts]
            (m/observe (fn [!]
                         (let [! #(some-> (f %) !), opts (clj->js opts)]
                           (.addEventListener node typ ! opts)
                           #(.removeEventListener node typ ! opts))))
            #_(m/eduction (filter some?) (listen* node typ f opts)))))

(defn uf->is [uf]
  (m/ap (m/amb (i/empty-diff 0)
          (let [!first (atom true) v (m/?> uf)]
            (assoc (i/empty-diff 1) :grow (if @!first (do (swap! !first not) 1) 0), :change {0 v})))))

(comment
  (def !! (atom nil))
  (def ps ((uf->is (m/observe (fn [!] (reset! !! !) #()))) #(prn :step) #(prn :done)))
  (def v [])
  (alter-var-root #'v i/patch-vec @ps)
  (@!! 5)
  )

(defn event->task [flow]
  (uf->is (m/ap
            (let [!busy? (atom false)
                  v (m/?> (m/eduction (remove (fn [_] @!busy?)) flow))
                  dfv (m/dfv), done! #(dfv false)]
              (m/amb
                [v done! (reset! !busy? true)]
                [v done! (reset! !busy? (m/? dfv))])))))

(defn event->tasks [flow]
  (uf->is
    (m/ap
      (let [S (i/spine)]
        (m/amb S
          (let [v (m/?> flow), id (random-uuid)]
            (S id {} [v #(S id {} nil)])
            (m/amb)))))))
