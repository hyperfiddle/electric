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


# About cljdoc

Cljdoc always try to generate documentation for all clojars deployments.
It will load and analyze all namespaces on the classpath.
Namespaces tagged with ^:no-doc will not generate documentation, but they will still be loaded.

Cljdoc requires:
- that all namespaces on the classpath be loadable (as per clojure.core/require),
- that all eventual dependencies required in classpath be listed in pom.xml.

Consequently, dependencies for all namespaces on classpath must be listed in the
**production** release's pom. This is inconvenient because we ship optional
namespaces in the prod artifact, like `contrib.datomic` and
`contrib.missionary-core-async` but we don't want to force a dependency on
datomic or core.async onto users.

To mitigate this issue, [cljdoc advises to list optional dependencies in our
pom.xml as `<scope>provided</scope>`](https://github.com/cljdoc/cljdoc/blob/94cbf3c357695a5a32e3c81a5f2bcdb77b3e2436/doc/userguide/for-library-authors.adoc#getting-dependencies-right),
BUT `clojure.deps` and `tools.build` do not support scopes.
Cljdoc advises to manually edit our pom.xml. We find this too error-prone to be acceptable.

We adapted our build script to generate the correct pom.xml. The generated pom
will production deps + all deps listed under the `:cljdoc-extra-deps` alias.
These deps should be tagged with `:pom/scope "provided"`:

```clojure
{:aliases {:cljdoc-extra-deps {:extra-deps {org.clojure/core.async {:mvn/version "1.6.681", :pom/scope "provided"}}}}}
```

`:pom/scope` is not supported by clojure.deps nor tools.build. Our build script interprets it.

## cljdoc and clojurescript analyzis

cljdoc analyzes cljs and cljc files with cljs.analyzer to detect all defs after
macroexpansion. As of today, cljdoc fails to analyze files containing e/defns.
For now cljdoc do not list cljs namespaces and cljs-only vars.

