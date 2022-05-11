# Hyperfiddle Photon

# Setup
```bash
git clone ...
git submodule update --init --recursive
pushd vendor/missionary && mvn clean && mvn compile && mvn install && popd
```

# Examples to run
* `clj -X:devkit :main user.orders-ui/main`
* `clj -A:dev -X user.photon-5-entrypoint/main`


