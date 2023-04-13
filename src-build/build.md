# Build clojars maven artifact

* versioning scheme is: `v2-alpha-0-g9aae7b1b`
* `v2`: brand/marketing major version, prefixed by the literal 'v'
* `alpha`: maturity in {alpha,beta,rc}; see https://clojure.org/releases/devchangelog 
* `0`: number of commits since the tag
* `g9aae7b1b`: git sha, prefixed by the literal 'g'


```shell
git tag v2-alpha  # manually set a new tag, or skip to use commit distance from current tag
clojure -T:build clean && rm -rf ./resources/public/js
#clj -A:dev -T user/release -- distribute sources, lib consumer will build
HYPERFIDDLE_ELECTRIC_BUILD=`git describe --tags --long --always --dirty`
clojure -T:build jar :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
clojure -T:build install :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
clj -A:dev -T user/main :replace-deps '{:deps {com.hyperfiddle/electric {:mvn/version "'$HYPERFIDDLE_ELECTRIC_BUILD'"}}}' # test demos with maven version
# No way to test remote clojars version without rm in .m2/repositories/com/hyperfiddle
# Optional: test electric-starter-app with local maven install
env $(cat .env | xargs) clojure -T:build deploy :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
```

- `CLOJARS_USERNAME` is your clojars username.
- `CLOJARS_PASSWORD` is not your account password, but rather a genareted token granting
deploy rights to the target coordinates.
- idea: how to run tests cli? (No need, deployed artifacts already passed CI)

# Continuous deployment of demos

```shell
clojure -T:build build-client          # optimized release build
clojure -T:build uberjar               # contains demos and demo server, currently
java -jar target/electric-demos-<version>-standalone.jar clojure.main -m prod
docker build --build-arg HYPERFIDDLE_ELECTRIC_SERVER_VERSION=$(git describe --tags --long --always --dirty) -t electric .
NO_COLOR=1 flyctl deploy --build-arg HYPERFIDDLE_ELECTRIC_SERVER_VERSION=$(git describe --tags --long --always --dirty)
```

- `NO_COLOR=1` disables docker-cli fancy shell GUI, so that we see the full log (not paginated) in case of exception
- `--build-only` tests the build on fly.io without deploying

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
