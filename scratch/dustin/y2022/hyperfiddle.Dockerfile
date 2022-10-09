# ------------------------------------------------------------------------------
# clojure dependencies
#
# Download dependenciesfrom clojure
#
# Yields:
#  /root/.m2
# ------------------------------------------------------------------------------
FROM clojure:openjdk-11-tools-deps AS clojure-deps
WORKDIR /app
COPY deps.edn deps.edn.tmp

# Enable for pro
# RUN mkdir -p /root/.m2/
# COPY settings.xml /root/.m2/settings.xml

# Hyperfiddle local repo isn't available at this stage, so don't resolve it's dep
RUN clojure -e "(spit \"deps.edn\" (-> \"deps.edn.tmp\" slurp read-string (update :deps dissoc 'com.hyperfiddle/hyperfiddle)))"
RUN clojure -e :ok

# ------------------------------------------------------------------------------
# shadow-cljs dependencies
#
# Download all dependencies needed to run shadow-cljs
# Works like an npm install shadow-cljs
# This step is required since shadow-cljs dependencies
#  are not listed in deps.edn
#
# Yields:
#  /root/.m2
# ------------------------------------------------------------------------------

FROM clojure:openjdk-11-tools-deps AS shadow-cljs-deps
WORKDIR /app
COPY --from=clojure-deps /app/deps.edn /app/deps.edn
COPY --from=clojure-deps /root/.m2 /root/.m2
COPY shadow-cljs.edn shadow-cljs.edn

RUN clojure -e "(require 'shadow.cljs.devtools.api)"

# ------------------------------------------------------------------------------
# node dependencies
#
# Download node dependencies via Yarn
#
# Yields:
#  node_modules
# ------------------------------------------------------------------------------
FROM node:14.7-stretch AS node-dependencies
WORKDIR /app
COPY package.json package.json
RUN yarn install

# ------------------------------------------------------------------------------
# node
#
# Parcel build
#
# Yields:
#  .serve
# ------------------------------------------------------------------------------
FROM node:14.7-stretch AS node-build
WORKDIR /app

COPY --from=node-dependencies /app/node_modules /app/node_modules

# selectively copy src files to avoid cache replication (.shadow-cljs)
COPY package.json package.json
COPY assets assets
COPY resources resources
COPY src src

RUN npx parcel build assets/browser.js --out-dir ./.serve --public-url ./

# ------------------------------------------------------------------------------
# clojure
#
# Build the uberjar, and add a .git_describe.txt for use in route.
#
# Yields:
#  project.jar
#  .git_describe.txt
# ------------------------------------------------------------------------------
FROM clojure:openjdk-11-tools-deps AS clojure-build
WORKDIR /app

## deps
COPY --from=shadow-cljs-deps /root/.m2 /root/.m2
COPY --from=node-build /app/node_modules /app/node_modules
COPY --from=node-build /app/.serve /app/.serve

### Include submodules
COPY .gitmodules .gitmodules
COPY hyperfiddle hyperfiddle
COPY vendor vendor

# config
COPY shadow-cljs.edn shadow-cljs.edn
COPY deps.edn deps.edn

## source
COPY src src

## resources
COPY assets assets
COPY resources resources

### generate git description
COPY .git .git
RUN git describe --tags --long --always --dirty > .git_describe.txt

# Build!
RUN clojure -A:free:cc -m compile-cljs :prod
RUN clojure -A:package-free

# ------------------------------------------------------------------------------
# package
#
# Builds the final docker image
#
# ------------------------------------------------------------------------------
FROM openjdk:11 AS package
WORkDIR /app

## Copy requirements from metal and previous stages
COPY --from=clojure-build /app/.serve /app/.serve
COPY --from=clojure-build /app/.git_describe.txt /app/.git_describe.txt
COPY --from=clojure-build /app/target/project.jar /app/target/project.jar
COPY ./hyperfiddle.edn .

## Expose points
EXPOSE 8080
CMD GIT_DESCRIBE=$(cat .git_describe.txt) java -cp target/project.jar clojure.main -m prod