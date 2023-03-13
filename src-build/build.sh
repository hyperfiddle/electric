#!/bin/sh

clojure -T:build clean
#clj -A:dev -X user/release -- distribute sources, lib consumer will build
HYPERFIDDLE_ELECTRIC_BUILD=`git describe --tags --long`
clojure -X:build jar :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
clojure -X:build install :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'