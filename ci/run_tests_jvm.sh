#!/bin/bash

echo "Running JVM tests"
clojure -X:test \
    :dirs "[\"src\" \"test\"]"  \
    :patterns                                \
    "[\"hyperfiddle.electric.impl.*\"        \
      \"hyperfiddle.incseq.*-test\"          \
      \"hyperfiddle.electric-test\"          \
      \"hyperfiddle.electric-de-test\"       \
      \"hyperfiddle.zero\"                   \
      \"hyperfiddle.missionary-test\"        \
      \"contrib.missionary-contrib-test\"    \
      \"contrib.ednish\"                     \
      \"contrib.sexpr-router\"               \
     ]"
