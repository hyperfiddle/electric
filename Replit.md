# How to run demos in Repl.it

## 1 - Start a REPL
In the `console` tab, run:
```shell
clj -A:dev
```
You should see:
```shell
Clojure 1.11.1
user=> 
```

## 2 - Compile the app
Type `(compile)` and press enter. You should see:
```clojure
user=> (compile)
[:devkit] Compiling ...
15:39:19.677 INFO  dev [pool-1-thread-2] - Initializing Test Database
[:devkit] Build completed. (232 files, 0 compiled, 0 warnings, 14.10s)
:done
user=> 
```
Compiling can take some time.

## 3 - Run the server

Type `(serve!)` and press enter.

A tab will pop with running demos.
Photon demos sources are located in `src-docs/user`.
