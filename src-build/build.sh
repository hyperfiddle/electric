#!/bin/sh

clojure -T:build clean
HYPERFIDDLE_ELECTRIC_BUILD=`git describe --tags --long --always --dirty`
clojure -T:build jar :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
clojure -T:build install :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
