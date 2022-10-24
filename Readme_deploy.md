# HOWTO deploy photon demo app

* prod demo entrypoint: `clojure -A:dev -X prod/main` (compiles photon at runtime, fixme)

# System setup
* Docker Desktop (includes CLI and bundled VM provider)

# Docker

docker login
docker ps
docker build -t hyperfiddle/photon-demo .
docker run -dP hyperfiddle/photon-demo
docker run -it hyperfiddle/photon-demo bash
docker push hyperfiddle/photon-demo:latest
docker build --platform linux/amd64 -t hyperfiddle/photon-demo .
docker buildx build --platform linux/amd64,linux/arm64 -t hyperfiddle/photon-demo .


# Jamsocket

npx jamsocket create photon-demo
npx jamsocket service create photon-demo
npx jamsocket push photon-demo hyperfiddle/photon-demo:latest
npx jamsocket spawn photon-demo

# Fly.io

brew install flyctl
fly auth signup
fly auth login
fly launch # wizard to create new project, go to https://fly.io/dashboard/personal from here out
...
fly deploy

cost = $41/mo for dedicated-cpu-1x, 4GB ram -- https://fly.io/docs/about/pricing/

# Digital Ocean and other VPS provider

# Dockerfile tips
* https://hub.docker.com/_/clojure
* Apple Silicon requires --platform linux/amd64
