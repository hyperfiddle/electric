(ns contrib.stacktrace
  "Tools for parsing and pretty printing error stack traces. Port of goog.testing.stacktrace.

  goog.testing.stacktrace is not available under advanced compilation without
  some hard-to-argue configuration (allowing test and debug code in prod). We
  want the `canonicalize` function who unifies stack traces representations
  between browsers. We use `canonicalize` to augment native stack traces with
  our async stack traces.

  This namespace is a port of `goog.testing.stacktrace` except:
  - it only implements `canonicalize`
  - `canonicalize` has been simplified to our use case (it doesn't strip goog.testing frames out, we don't use goog.testing)
  - we don't support deobfuscation

  Essentially, this namespace is
  - a collection of regexps,
  - a parse function
  - a toString function

  canonicalize:  native stack trace -> parse -> toString -> canonical stack trace
  "
  ;; Original source:
  ;; https://github.com/google/closure-library/blob/7818ff7dc0b53555a7fb3c3427e6761e88bde3a2/closure/goog/testing/stacktrace.js#L12
  (:require [clojure.string :as str]
            [hyperfiddle.rcf :as rcf]))

(defn ->frame
  "
`context` Context object, empty in case of global functions or if the browser doesn't provide this information.
`name` Function name, empty in case of anonymous functions.
`alias` Alias of the function if available. For example the function name will be 'c' and the alias will be 'b' if the function is defined as <code>a.b = function c() {};</code>.
`path` File path or URL including line number and optionally column number separated by colons.
"
 [context name alias path]
  {::context context
   ::name    name
   ::alias   alias
   ::path    path})

(defn anonymous? "Whether the stack frame contains an anonymous function." [frame]
  (or (not (::name frame))
    (= "[object Object]" (::context frame))))

(declare html-escape maybe-deobfuscate-function-name)

(defn to-canonical-string
  "Brings one frame of the stack trace into a common format across browsers."
  [{::keys [context name alias path]}]
  (str/join "" [(if (not-empty context)
                  (str (html-escape context) ".")
                  "")
                (if (not-empty name)
                  (html-escape (maybe-deobfuscate-function-name name))
                  "anonymous")
                (if (not-empty alias)
                  (str " [as " (html-escape (maybe-deobfuscate-function-name alias)) "]")
                  "")
                (when (not-empty path)
                  (str " at " (html-escape path)))]))

(def IDENTIFIER-PATTERN "RegExp pattern for JavaScript identifiers. We don't support Unicode identifiers defined in ECMAScript v3."
  "[a-zA-Z_$][\\w$]*")

(def V8-ALIAS-PATTERN "RegExp pattern for function name alias in the V8 stack trace."
  (str "(?: \\[as (" IDENTIFIER-PATTERN ")\\])?"))

(def V8-CONTEXT-PATTERN "RegExp pattern for the context of a function call in a V8 stack trace. Creates an optional submatch for the namespace identifier including the 'new' keyword for constructor calls (e.g. 'new foo.Bar')."
  (str "(?:((?:new )?(?:\\[object Object\\]|"
    IDENTIFIER-PATTERN "(?:\\."
    IDENTIFIER-PATTERN ")*))\\.)?"))

(def V8-FUNCTION-NAME-PATTERN "RegExp pattern for function names and constructor calls in the V8 stack trace."
  (str "(?:new )?(?:" IDENTIFIER-PATTERN "|<anonymous>)"))

(def V8-FUNCTION-CALL-PATTERN "RegExp pattern for function call in the V8 stack trace. Creates 3 submatches with context object (optional), function name and function alias (optional)."
  (str " " V8-CONTEXT-PATTERN
    "(" V8-FUNCTION-NAME-PATTERN ")"
    V8-ALIAS-PATTERN))

(def URL-PATTERN "RegExp pattern for an URL + position inside the file."
  "((?:http|https|file)://[^\\s)]+|javascript:.*)")

(def CHROME-URL-PATTERN "RegExp pattern for an URL + line number + column number in V8. The URL is either in submatch 1 or submatch 2."
  (str " (?:"
    "\\(unknown source\\)"
    "|"
    "\\(native\\)"
    "|"
    "\\((.+)\\)|(.+))"))

(def V8-STACK-FRAME-REGEXP "Regular expression for parsing one stack frame in V8. For more information on V8 stack frame formats, see https://code.google.com/p/v8/wiki/JavaScriptStackTraceApi."
  (new js/RegExp (str "^    at"
                   "(?:" V8-FUNCTION-CALL-PATTERN ")?"
                   CHROME-URL-PATTERN "$")))

(def FIREFOX-FUNCTION-CALL-PATTERN
  "RegExp pattern for function call in the Firefox stack trace. Creates 2 submatches with function name (optional) and arguments.

Modern FF produces stack traces like:
    foo@url:1:2
    a.b.foo@url:3:4
"
  (str "("
    IDENTIFIER-PATTERN "(?:\\."
    IDENTIFIER-PATTERN ")*"
    ")?"
    "(\\(.*\\))?@"))

(def FIREFOX-STACK-FRAME-REGEXP "Regular expression for parsing one stack frame in Firefox."
  (new js/RegExp (str "^" FIREFOX-FUNCTION-CALL-PATTERN "(?::0|"
                   URL-PATTERN ")$")))

(def OPERA-ANONYMOUS-FUNCTION-NAME-PATTERN
  "RegExp pattern for an anonymous function call in an Opera stack frame. Creates 2 (optional) submatches: the context object and function name."
  (str "<anonymous function(?:\\: "
    "(?:(" IDENTIFIER-PATTERN "(?:\\."
    IDENTIFIER-PATTERN ")*)\\.)?"
    "(" IDENTIFIER-PATTERN "))?>"))

(def OPERA-FUNCTION-CALL-PATTERN
  "RegExp pattern for a function call in an Opera stack frame. Creates 4 (optional) submatches: the function name (if not anonymous), the aliased context object and function name (if anonymous), and the function call arguments."
  (str "(?:(?:("
    IDENTIFIER-PATTERN ")|"
    OPERA-ANONYMOUS-FUNCTION-NAME-PATTERN
    ")(\\(.*\\)))?@"))

(def OPERA-STACK-FRAME-REGEXP
  "Regular expression for parsing on stack frame in Opera 11.68 - 12.17. Newer versions of Opera use V8 and stack frames should match against `V8-STACK-FRAME-REGEXP`"
  (str "^" OPERA-FUNCTION-CALL-PATTERN URL-PATTERN "?$"))

(def IE-FUNCTION-CALL-PATTERN "RegExp pattern for function call in a IE stack trace. This expression allows for identifiers like 'Anonymous function', 'eval code', and 'Global code'."
  (str "("
    IDENTIFIER-PATTERN "(?:\\."
    IDENTIFIER-PATTERN ")*"
    "(?:\\s+\\w+)*)"))

(def IE-STACK-FRAME-REGEXP "Regular expression for parsing a stack frame in IE."
  (new js/RegExp (str "^   at " IE-FUNCTION-CALL-PATTERN "\\s*\\("
                   "("
                   "eval code:[^)*"
                   "|"
                   "Unknown script code:[^)]*"
                   "|" URL-PATTERN ")\\)?$")))

(defn parse-stack-frame "Parses one stack frame." [^js frame-str]
  ;; This match includes newer version of Opera (15+)
  (if-let [m (.match frame-str V8-STACK-FRAME-REGEXP)]
    (->frame
      (or (aget m 1) "")
      (or (aget m 2) "")
      (or (aget m 3) "")
      (or (aget m 4) (aget m 5) (aget m 6) ""))
    (if-let [m (.match frame-str FIREFOX-STACK-FRAME-REGEXP)]
      (->frame "", (or (aget m 1) ""), "", (or (aget m 3) ""))
      ;; Match against Presto Opera 11.68 - 12.17.
      (if-let [m (.match frame-str OPERA-STACK-FRAME-REGEXP)]
        (->frame (or (aget m 2) ""), (or (aget m 1) (aget m 3) ""), "", (or (aget m 5) ""))
        (if-let [m (.match frame-str IE-STACK-FRAME-REGEXP)]
          (->frame "", (or (aget m 1) ""), "", (or (aget m 2) ""))
          nil)))))

(defn maybe-deobfuscate-function-name [name] name) ; We don't support deobfuscation.

(defn html-escape "Escapes the special character in HTML."
  [text]
  (-> text
    (str/replace #"&" "&amp")
    (str/replace #"<" "&lt;")
    (str/replace #">" "&gt;")
    (str/replace #"\"" "&quot;")
    ))

(defn frames-to-string
  "Converts the stack frames into canonical format."
  [frames]
  (->> frames
    (map to-canonical-string)
    (map #(str "> " %))
    (str/join "\n")))

(defn parse "Parses the browser's native stack trace." [stack]
  (let [lines (str/split-lines (str/replace stack #"\s*$" ""))]
    (map parse-stack-frame lines)))

(defn canonicalize "Brings the stack trace into a common format across browsers."
  [stack]
  (frames-to-string (parse stack)))

(rcf/tests
  ;; obtained with `var s; try {cljs.core.apply(null, [])} catch(err){s = err.stack};`
  (def ff-trace "cljs.core.apply.cljs$core$IFn$_invoke$arity$2@http://localhost:8080/js/cljs-runtime/cljs.core.js:13679:4\ncljs$core$apply@http://localhost:8080/js/cljs-runtime/cljs.core.js:13641:24\n@debugger eval code:1:23")
  (canonicalize ff-trace) := "> cljs.core.apply.cljs$core$IFn$_invoke$arity$2 at http://localhost:8080/js/cljs-runtime/cljs.core.js:13679:4\n> cljs$core$apply at http://localhost:8080/js/cljs-runtime/cljs.core.js:13641:24\n> anonymous"

  (def chrome-trace "TypeError: Cannot read properties of null (reading 'cljs$lang$applyTo')\n    at Function.cljs$core$IFn$_invoke$arity$2 (http://localhost:8080/js/cljs-runtime/cljs.core.js:13679:6)\n    at Object.cljs$core$apply [as apply] (http://localhost:8080/js/cljs-runtime/cljs.core.js:13641:24)\n    at <anonymous>:1:23")
  (canonicalize chrome-trace) := "> anonymous\n> Function.cljs$core$IFn$_invoke$arity$2 at http://localhost:8080/js/cljs-runtime/cljs.core.js:13679:6\n> Object.cljs$core$apply [as apply] at http://localhost:8080/js/cljs-runtime/cljs.core.js:13641:24\n> anonymous at &lt;anonymous&gt;:1:23"

  (def safari-trace "@http://localhost:8080/js/cljs-runtime/cljs.core.js:13679:5\ncljs$core$apply@http://localhost:8080/js/cljs-runtime/cljs.core.js:13641:53\nglobal code@\nevaluateWithScopeExtension@[native code]\n@[native code]\n_wrapCall@[native code]")
  (canonicalize safari-trace) := "> anonymous at http://localhost:8080/js/cljs-runtime/cljs.core.js:13679:5\n> cljs$core$apply at http://localhost:8080/js/cljs-runtime/cljs.core.js:13641:53\n> anonymous\n> anonymous\n> anonymous\n> anonymous"
  )
