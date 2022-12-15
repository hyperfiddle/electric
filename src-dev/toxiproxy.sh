#!/bin/sh

# https://github.com/Shopify/toxiproxy
# brew tap shopify/shopify
# brew install toxiproxy

LOG_LEVEL=error toxiproxy-server & \
toxiproxy-cli create --listen localhost:$1 --upstream localhost:$2 hf_toxic_proxy && \
toxiproxy-cli toxic add -t latency -a latency=$3 hf_toxic_proxy

# ./toxiproxy.sh 8083 8080 500
# killall toxiproxy-server
