# Clojure/Script compatibiltiy matrix — Electric Clojure

We target full Clojure/Script compatibility (say 99%). That means you can take a pre-existing Clojure snippet and copy/paste it into an Electric function body and it will "work" and produce the correct result. Including host interop syntax, use of pre-existing macros, etc.

Gaps:

- reactive multimethods
- reactive protocols
- ...