#!/bin/bash

echo "Running JVM tests"
clojure -X:test \
    :dirs "[\"src\" \"src-docs\" \"test\"]"  \
    :patterns                                \
    "[\"hyperfiddle.photon-impl.*\"          \
      \"hyperfiddle.photon-test\"            \
      \"hyperfiddle.missionary-test\"        \
      \"hyperfiddle.logger-test\"            \
      \"hyperfiddle.core-async-test\"]"
