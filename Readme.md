# Fabric

```bash
git submodule update --init --recursive
npm install -g typescript shadow-cljs
npm install

# in one shell
tsc --watch

# in another shell
npm run server

# in another shell / ide
clj -A:server

```

Then go to `http://localhost:9630` on the `Builds` tab to start your build.
This is where you are going to see build info, errors, etc…

The run the server, go to `hyperfiddle.server.core` and use your REPL to run the
comment block at the bottom of the file.

https://blog.janestreet.com/breaking-down-frp/
