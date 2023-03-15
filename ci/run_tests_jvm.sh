#!/bin/bash

echo "Running JVM tests"
clojure -X:test \
    :dirs "[\"src\" \"src-docs\" \"test\"]"  \
    :patterns                                \
    "[\"hyperfiddle.electric.impl.*\"          \
      \"hyperfiddle.electric-test\"            \
      \"hyperfiddle.zero\"                   \
      \"hyperfiddle.missionary-test\"        \
      \"contrib.missionary-contrib-test\"    \
      \"contrib.ednish\"                     \
      \"contrib.sexpr-router\"               \
     ]"
