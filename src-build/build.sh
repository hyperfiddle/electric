#!/bin/sh

clojure -X:build clean && rm -rf ./resources/public/js
#clj -A:dev -X user/release -- distribute sources, lib consumer will build
HYPERFIDDLE_ELECTRIC_BUILD=`git describe --tags --long`
clojure -X:build jar :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
clojure -X:build install :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'