; The Clojure compiler does not automatically require reader functions referred to in
; data_readers.clj/c, but the Clojurescript compiler does.
; https://www.clojurescript.org/about/differences

{uri #?(:clj contrib.uri/uri-clj-reader :cljs contrib.uri/uri-cljs-reader)
 #?@(:cljs (long goog.math.Long/fromString))} ; see user.datomic-contrib
