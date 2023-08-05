(ns dev)

(comment
  "Performance profiling, use :profile deps alias"
  (require '[clj-async-profiler.core :as prof])
  (prof/serve-files 8082)
  ;; Navigate to http://localhost:8082
  (prof/start {:framebuf 10000000})
  (prof/stop))
