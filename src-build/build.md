# Build instructions

* Electric contains <100 LOC of Java that must be compiled - see `src/hyperfiddle/electric/{Failure,Pending,Remote}.java`
* Since the java code rarely changes, we commit compiled .class artifacts directly into the repo.
* As of 2023 Feb 10, we still target java8 compatibility. (Note our jetty adapter also targets Java 8 compat).

# Java compile steps

```
$ clojure -T:build compile-java
warning: [options] bootstrap class path not set in conjunction with -source 8
```
The above warning is expected and can be ignored.