#!/bin/sh

clojure -T:build clean && rm -rf ./resources/public/js
#clj -A:dev -X user/release -- distribute sources, lib consumer will build
HYPERFIDDLE_ELECTRIC_BUILD=`git describe --tags --long --always --dirty`
clojure -T:build jar :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
clojure -T:build install :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'