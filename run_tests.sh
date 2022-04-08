#!/bin/bash

clojure -X:test :dirs "[\"src\" \"src-docs\" \"test\"]" :patterns "[\"hyperfiddle.photon-impl.compiler\" \"hyperfiddle.photon-impl.for\" \"hyperfiddle.photon-test\"]"
