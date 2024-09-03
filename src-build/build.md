# Build clojars maven artifact

* versioning scheme is: `v3-alpha-0-g9aae7b1b`
* `v3`: brand/marketing major version, prefixed by the literal 'v'
* `alpha`: maturity in {alpha,beta,rc}; see https://clojure.org/releases/devchangelog
* `0`: number of commits since the tag
* `g9aae7b1b`: git sha, prefixed by the literal 'g'


```shell
git tag v3-alpha  # manually set a new tag, or skip to use commit distance from current tag
clojure -T:build clean
#clj -A:dev -T user/release -- distribute sources, lib consumer will build
HYPERFIDDLE_ELECTRIC_BUILD=`git describe --tags --long --always --dirty`
clojure -T:build jar :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
clojure -T:build install :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
# To test in electric-starter-app:
clj -A:dev -X dev/-main -Sdeps '{:deps {com.hyperfiddle/electric {:mvn/version "'$HYPERFIDDLE_ELECTRIC_BUILD'"}}}'
# No way to test remote clojars version without rm in .m2/repositories/com/hyperfiddle
# Optional: test electric-starter-app with local maven install
env $(cat .env | xargs) clojure -T:build deploy :version '"'$HYPERFIDDLE_ELECTRIC_BUILD'"'
```

- `CLOJARS_USERNAME` is your clojars username.
- `CLOJARS_PASSWORD` is not your account password, but rather a genareted token granting
deploy rights to the target coordinates.
- idea: how to run tests cli? (No need, deployed artifacts already passed CI)
