# Hyperfiddle Photon

# Setup
```bash
git clone ...
cd photon
git submodule update --init --recursive
brew install maven
pushd vendor/missionary && mvn clean && mvn compile && mvn install && popd
yarn       # or npm install
```

Sanity check that it's working: `clj -A:dev -X user.photon-5-entrypoint/main`

# Photon Tutorial
* Start a JVM REPL and jack-in in your usual way
* Read `src-docs/user/photon_*`, follow along in your REPL

# Standalone Examples to run
* (wip) tee-shirt orders: `clj -X:devkit :main user.orders-ui/main`
