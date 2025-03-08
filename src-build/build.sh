#!/bin/sh
set -eux -o pipefail

# Todo clojure build output needs to log diagnostics to stderr (like set -x) so that
# stdout can be used to return a value, such as the _BUILD value, to another script.

clojure -T:build clean
HYPERFIDDLE_ELECTRIC_BUILD="v3-alpha-SNAPSHOT"
clojure -T:build jar :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
clojure -T:build install :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
#env $(cat .env | xargs) clojure -T:build deploy :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
#env $(cat .env | xargs) clojure -T:build deploy :version '"'v3-alpha-SNAPSHOT'"'