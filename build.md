# Deploy steps

## Clean
```shell
clojure -T:build clean
```

## Build

Versioning scheme is the artifact build time in UTC:
`v<year><month><day>-<hour><minute><second>` like `v20220727-091415`.

```shell
HYPERFIDDLE_PHOTON_DATE=`date -u '+%Y%m%d-%H%M%S'`
rm -rf ./resources/public/js
clj -T:build clean
clojure -T:build jar :version '"'$HYPERFIDDLE_PHOTON_DATE'"'
git tag v$HYPERFIDDLE_PHOTON_DATE
```

## Install jar in local maven repo
```shell
clojure -T:build install :version '"'$HYPERFIDDLE_PHOTON_DATE'"'
```

## Test local maven repo
```clojure
{:deps {org.clojure/clojure    {:mvn/version "1.11.1"}
        com.hyperfiddle/photon {:mvn/version "$HYPERFIDDLE_PHOTON_DATE"}}} ; replace value
```

## Deploy

```shell
CLOJARS_USERNAME=dustingetz CLOJARS_PASSWORD=<token> clojure -T:build deploy :version '"'$HYPERFIDDLE_PHOTON_DATE'"'
```

- `CLOJARS_USERNAME` is your account username.
- `CLOJARS_PASSWORD` is not your account password, but a genareted token granting
deploy rights to the target coordinates. Ask an Hyperfiddle admin to generate
one for you.


## Check deployment
- `rm -rf ~/.m2/repository/com/hyperfiddle/photon/"$HYPERFIDDLE_PHOTON_DATE"/`
- test again
