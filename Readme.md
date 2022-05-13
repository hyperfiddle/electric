# Hyperfiddle Photon

# Setup
```bash
git clone ...
git submodule update --init --recursive
pushd vendor/missionary && mvn clean && mvn compile && mvn install && popd
npm install
```

# Standalone Examples to run
* Tee-shirt orders: `clj -X:devkit :main user.orders-ui/main`

# Photon Tutorial
* Start a JVM REPL and jack-in in your usual way
* Read `src-docs/user/photon_*`, follow along in your REPL
* 
