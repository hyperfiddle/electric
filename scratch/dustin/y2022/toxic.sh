#!/bin/sh

# $ brew tap shopify/shopify
# $ brew install toxiproxy

toxiproxy-server & \
toxiproxy-cli create --listen localhost:$1 --upstream localhost:$2 hf_toxic_proxy && \
toxiproxy-cli toxic add -t latency -a latency=150 hf_toxic_proxy

# killall toxyproxy-server
