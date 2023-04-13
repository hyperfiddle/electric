FROM clojure:openjdk-11-tools-deps AS clojure-deps
WORKDIR /app
COPY deps.edn deps.edn
COPY src-build src-build
RUN clojure -A:dev -M -e :ok        # preload deps
RUN clojure -T:build noop           # preload build deps

FROM node:14.7-stretch AS node-deps
WORKDIR /app
COPY package.json package.json
RUN yarn install

FROM clojure:openjdk-11-tools-deps AS build
WORKDIR /app
COPY --from=node-deps /app/node_modules /app/node_modules
COPY --from=clojure-deps /root/.m2 /root/.m2
COPY shadow-cljs.edn shadow-cljs.edn
COPY deps.edn deps.edn
COPY src src
COPY src-prod src-prod
COPY src-docs src-docs
COPY src-build src-build
COPY resources resources
ARG REBUILD=unknown
ARG HYPERFIDDLE_ELECTRIC_SERVER_VERSION
RUN clojure -X:build uberjar :jar-name "app.jar" :verbose true :version $HYPERFIDDLE_ELECTRIC_SERVER_VERSION

FROM amazoncorretto:11 AS app
WORKDIR /app
COPY --from=build /app/app.jar app.jar
# not required - included for Directory Explorer demo
COPY --from=node-deps /app/node_modules node_modules
EXPOSE 8080
ARG HYPERFIDDLE_ELECTRIC_SERVER_VERSION
ENV HYPERFIDDLE_ELECTRIC_SERVER_VERSION=$HYPERFIDDLE_ELECTRIC_SERVER_VERSION
CMD java -cp app.jar -DHYPERFIDDLE_ELECTRIC_SERVER_VERSION=$HYPERFIDDLE_ELECTRIC_SERVER_VERSION clojure.main -m prod
