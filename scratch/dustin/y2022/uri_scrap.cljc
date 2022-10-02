

(defn print-uri [o w]
  {:pre [(uri? o)]}
  (let [str-rep (cond
                  #?@(:clj (#_#_(instance? com.cognitect.transit.URI o) (.getValue o)
                            (instance? java.net.URI o) (.toString o))
                      :cljs (#_#_(cognitect.transit/uri? o) (.-rep o) ; tagged value
                             (instance? goog.Uri o) (.-str-rep o))))]
    (#?(:clj .write :cljs -write) w (str "#uri \"" str-rep "\""))))



;#?(:cljs (deftype URI [uri-str]
;           Object (toString [_] uri-str)
;           IPrintWithWriter (-pr-writer [o writer _] (print-uri o writer))
;           IHash (-hash [this] (hash uri-str))
;           IEquiv (-equiv [this other] (and (instance? URI other) (= (.-uri-str this) (.-uri-str other))))
;           IComparable (-compare [this other] (and (instance? URI other) (compare (.-uri-str this) (.-uri-str other))))))


;(defn uri? [o]
;  #?(:clj (or #_(instance? java.net.URI o)
;              (instance? com.cognitect.transit.URI o))
;     :cljs (or (cognitect.transit/uri? o)
;               (instance? URI o))))


#?(:cljs (extend-type com.cognitect.transit.types/TaggedValue
           ;Object (toString [o] (.-rep o)) ; ?
           IPrintWithWriter (-pr-writer [o writer _] (if (cognitect.transit/uri? o)
                                                       (print-uri o writer)
                                                       #_(pr-writer o writer (pr-opts)))))) ; unfortunately private

(tests
  (def x #?(:cljs (cognitect.transit/uri "http://localhost:8080/a?b#c")
            :clj (java.net.URI. "http://localhost:8080/a?b#c")))
  #?@(:cljs ((pr-str x) := )
      :clj ((pr-str x) := "#uri \"http://localhost:8080/a?b#c\"")) ; ? java.net.URI str expected..

  (pr-str x) := "#object[Transit$TaggedValue [TaggedValue: r, http://localhost:8080/a?b#c]]" ; before
  (str x) := "[TaggedValue: r, http://localhost:8080/a?b#c]"
  (pr-str x) := "#uri \"http://localhost:8080/a?b#c\""

  #?(:cljs
     (let [sb (goog.string.StringBuffer.)
           writer (StringBufferWriter. sb)]
       (-pr-writer o writer (pr-opts))
       (-flush writer)
       (str sb)))

  #?(:cljs
     (let [sb (goog.string.StringBuffer.)
           writer (StringBufferWriter. sb)]
       (print o writer (pr-opts))
       (-flush writer)
       (str sb)))

  #?@(:cljs ((cognitect.transit/uri? x) := true)
      :clj ()))