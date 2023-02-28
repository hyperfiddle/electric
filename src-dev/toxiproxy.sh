#!/bin/sh

### INSTALLATION

# https://github.com/Shopify/toxiproxy
# brew tap shopify/shopify
# brew install toxiproxy

### RUNNING

# ./toxiproxy.sh 8083 8080 200
# toxiproxy-cli list
# killall toxiproxy-server
# toxiproxy-cli toxic help
# toxiproxy-cli toxic update --toxicName hf_latency_toxic --attribute latency=500 hf_dev_proxy


log_level=fatal # even 'error' is too spammy

LOG_LEVEL=${log_level} toxiproxy-server &
echo "waiting for server to come up:"
while ! nc -z localhost 8474; do
    printf .
    sleep 1
done
echo " server up"
toxiproxy-cli create --listen 0.0.0.0:$1 --upstream localhost:$2 hf_dev_proxy && \
toxiproxy-cli toxic add --toxicName hf_latency_toxic --type latency --attribute latency=$3 hf_dev_proxy