# Build clojars maven artifact — Electric

Build version is to be set under `deps.edn` > `:hyperfiddle.build/version`.

```shell
clojure -T:build build
clojure -T:build install
# To test in electric-starter-app:
clj -A:dev -X dev/-main -Sdeps '{:deps {com.hyperfiddle/electric {:mvn/version "<installed version>"}}}'
# No way to test remote clojars version without rm in .m2/repositories/com/hyperfiddle
# Optional: test electric-starter-app with local maven install
env $(cat .env | xargs) clojure -T:build deploy
```

- `CLOJARS_USERNAME` is your clojars username.
- `CLOJARS_PASSWORD` is not your account password, but rather a genareted token granting
deploy rights to the target coordinates.
- idea: how to run tests cli? (No need, deployed artifacts already passed CI)
