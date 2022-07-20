Build
```shell
HYPERFIDDLE_PHOTON_DATE=`date '+%Y%m%d'` 
rm -rf ./resources/public/js
clj -T:build clean
clojure -T:build jar :version '"'$HYPERFIDDLE_PHOTON_DATE'"'
git tag v$HYPERFIDDLE_PHOTON_DATE
```

Clean 
`clojure -T:build clean`

Install jar in local maven repo:
`clojure -T:build install :version '"'$HYPERFIDDLE_PHOTON_DATE'"'`

Test local maven repo:
```clojure
{:deps    {org.clojure/clojure {:mvn/version "1.10.3"}
           com.hyperfiddle/rcf {:mvn/version "20220405"}}}

```

Deploy
```shell
CLOJARS_USERNAME=dustingetz CLOJARS_PASSWORD= clojure -T:build deploy :version '"'$HYPERFIDDLE_RCF_DATE'"'
```

rm local m2 cache
test again
