FROM clojure:openjdk-11-tools-deps AS clojure-deps
WORKDIR /app
COPY deps.edn deps.edn
RUN clojure -A:dev -M -e :ok

FROM node:14.7-stretch AS node-deps
WORKDIR /app
COPY package.json package.json
RUN yarn install

FROM clojure:openjdk-11-tools-deps AS shadow-cljs-build
WORKDIR /app
COPY --from=node-deps /app/node_modules /app/node_modules
COPY --from=clojure-deps /root/.m2 /root/.m2
COPY shadow-cljs.edn shadow-cljs.edn
COPY deps.edn deps.edn
COPY src src
COPY src-dev src-dev
COPY src-docs src-docs
COPY resources resources
RUN clojure -A:dev -X prod/build

FROM clojure:openjdk-11-tools-deps AS clojure-build
WORKDIR /app
COPY --from=clojure-deps /root/.m2 /root/.m2
COPY --from=node-deps /app/node_modules /app/node_modules
COPY --from=shadow-cljs-build /app/resources/public/js /app/resources/public/js
COPY shadow-cljs.edn shadow-cljs.edn
COPY deps.edn deps.edn
COPY src src
COPY src-dev src-dev
COPY src-docs src-docs
COPY resources resources
EXPOSE 8080
CMD clojure -A:dev -X prod/main
