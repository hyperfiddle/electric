
(defnode lineseg [])

(defnode flute [dur freq amp vfreq]
  (let [amp1 (linseg)
        amp2 (linseg)
        ampv (linseg)
        flow (rand 1 amp1)
        vibr (oscils vfreq (* 0.1 ampv))]
    (rec
      (let [x (delay (/ 1 freq 2) (+ (* breath flow) amp1 vibr feedbk))
            out (tone (+ x (- (* x x x)) feedbk) 2000)
            feedbk (* body .4)
            body (delay (/ 1 freq) out)]))
    (* out (* amp amp2))))

(defn example-system [config-options]
  (let [{:keys [host port]} config-options]
    (component/system-map
      :db (new-database host port)
      :scheduler (new-scheduler)
      :app (component/using
             (example-component config-options)
             {:database  :db
              :scheduler :scheduler}))))

(r/defn system [{:keys [host port] :as config}]
  (let [database (Database host port)
        scheduler (Scheduler config)]
    (Example-component database scheduler)))