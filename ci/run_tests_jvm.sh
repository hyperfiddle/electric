#!/bin/bash

echo "Running JVM tests"
clojure -X:test \
    :dirs "[\"src\" \"test\"]"  \
    :patterns                                \
    "[\"hyperfiddle.*-test\"                 \
      \"hyperfiddle.electric.impl.*\"        \
      \"contrib.((?!datomic).)*\"            \
     ]"

