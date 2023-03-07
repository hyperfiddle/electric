# Build clojars maven artifact

* versioning scheme is: `v2-alpha-0-g9aae7b1b`
* `v2`: brand/marketing major version, prefixed by the literal 'v'
* `alpha`: maturity in {alpha,beta,rc}; see https://clojure.org/releases/devchangelog 
* `0`: number of commits since the tag
* `g9aae7b1b`: git sha, prefixed by the literal 'g'


```shell
git tag v2-alpha  # manually set a new tag, or skip to use commit distance from current tag
clojure -T:build clean && rm -rf ./resources/public/js
#clj -A:dev -X user/release -- distribute sources, lib consumer will build
HYPERFIDDLE_ELECTRIC_BUILD=`git describe --tags --long`
clojure -T:build jar :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
clojure -T:build install :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
```

Test local maven repo:
```clojure
{:deps 
 {org.clojure/clojure {:mvn/version "1.11.1"}
  com.hyperfiddle/electric {:mvn/version $HYPERFIDDLE_ELECTRIC_BUILD}}}
```
```shell 
clj -A:dev -X user/main :replace-deps '{:deps {com.hyperfiddle/electric {:mvn/version "'$HYPERFIDDLE_ELECTRIC_BUILD'"}}}'
# how to run tests cli?
```

Clojars:

```shell
env $(cat .env | xargs) CLOJARS_USERNAME=dustingetz clojure -T:build deploy :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
```

- `CLOJARS_USERNAME` is your clojars username.
- `CLOJARS_PASSWORD` is not your account password, but rather a genareted token granting
deploy rights to the target coordinates.

Check deployment
- `rm -rf ~/.m2/repository/com/hyperfiddle/electric/"$HYPERFIDDLE_ELECTRIC_BUILD"/`
- test again

# Java build (skip unless Java code changes)

* Electric contains <100 LOC of Java that must be compiled
* see `src/hyperfiddle/electric/{Failure,Pending,Remote}.java`
* We commit compiled .class artifacts directly into the repo for these, because they rarely change.
* As of 2023 Feb 10, we still target java8 compatibility. (Note our jetty adapter also targets Java 8.)

```
$ clojure -T:build compile-java
warning: [options] bootstrap class path not set in conjunction with -source 8
```
The above warning is expected and can be ignored.


# Build client production bundle

```shell
clojure -X:build build-client
```
Will compile client program into a single `.js` file, optimized for production.


# Build an uberjar

```shell
clojure -X:build uberjar
```

Will build:
- a production-optimized client program
- an executable, self-contained jar, including:
  - sources,
  - resources,
  - the built client program

# Build a Docker image

```shell
docker build --build-arg VERSION=$(git describe --tags --long) -t electric .
```
