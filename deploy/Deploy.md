# Deploy

; compiles photon at runtime
clojure -A:dev -X prod/main



# Scratch

clojure -T:demo-uberjar clean
clojure -M:demo-uberjar
java -cp target/photon.jar clojure.main -m prod
clojure -A:demo-uberjar:dev:scratch

➜  photon git:(master) ✗ clojure -A:demo-uberjar:dev:scratch
WARNING: compile already refers to: #'clojure.core/compile in namespace: user, being replaced by: #'user/compile
Clojure 1.11.1
user=> (require 'build-demo)
[:devkit] Compiling ...
