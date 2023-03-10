#!/bin/sh

echo "Running NodeJS tests"
clojure -M:test:shadow-cljs compile :test --force-spawn
node out/node-tests.js
