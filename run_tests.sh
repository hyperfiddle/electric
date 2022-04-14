#!/bin/bash

clojure -X:test :dirs "[\"src\" \"src-docs\" \"test\"]" :patterns "[\"hyperfiddle.photon-impl.*\" \"hyperfiddle.photon-test\"]"
