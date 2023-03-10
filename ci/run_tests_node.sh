#!/bin/sh

echo "Running NodeJS tests"
clj -M:test:shadow-cljs compile :test --force-spawn
node out/node-tests.js
