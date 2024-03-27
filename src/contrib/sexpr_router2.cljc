(ns contrib.sexpr-router2
  (:require [clojure.string :as string]
            contrib.ednish2
            [hyperfiddle.rcf :refer [tests]]))

(defn encode [route & [home-route]]
  (let [[f & args] route]
    (condp = route
      home-route "/"
      (str "/" (contrib.ednish2/encode f)
           "/" (some->> args
                        (map contrib.ednish2/encode)
                        (string/join "/"))))))

(def url-regex #"^/([^/?#]+/?)*(\?[^#]*)?(#.*)?$")

(defn decode [s & [home-route]]
  {:pre [(string? s)]}
  (if-let [[match _ query hash] (re-find url-regex s)]
    (condp = match
      "/" home-route ; todo many edge cases
      (->> (cond-> match
                   (seq hash) (-> (string/split #"#") first) ; ?
                   (seq query) (-> (string/split #"\?") first)
                   true (string/split #"/"))
           (rest)
           (map contrib.ednish2/decode)))
    (throw (ex-info (str "Invalid url: " s) {::s s}))))

(tests
  "sexpr routes"
  (encode `(user/hello)) := "/user%2Fhello/"
  (decode "/user%2Fhello/") := `(user/hello)

  (encode `(user/hello :a 1 "alice")) := "/user%2Fhello/:a/1/%22alice%22"
  (decode "/user%2Fhello/:a/1/%22alice%22") := `(user/hello :a 1 "alice")

  "pass a home-route"
  (def home `(user/hello :default))
  (encode `(user/hello :default) home) := "/"
  (decode "/" home) := `(user/hello :default)
  (encode `(user/hello :a 1 "alice") home) := "/user%2Fhello/:a/1/%22alice%22"
  (decode "/user%2Fhello/:a/1/%22alice%22" home) := `(user/hello :a 1 "alice")

  (encode `(user/hello) home) := "/user%2Fhello/"
  (decode "/user%2Fhello/" home) := `(user/hello)

  "home-route edge cases"
  (def home `(user/hello))
  (decode "/" home) := `(user/hello)
  ;(decode "/?a" home) := `(user/hello)
  ;(decode "/#a" home) := `(user/hello)
  ;(decode "//" home) := `(user/hello)
  ;(decode "//a" home) := `(user/hello)

  "routes must pass java.net.URI"
  #?(:clj (some? (java.net.URI. (encode `(user/hello)))) := true)

  "non-ascii characters (note: ednish is not a bijection)"
  (encode '(foo "!$&'[]()*+,;=|")) := "/foo/%22%21$&%27()()*%2B,;=%7C%22"
  (decode "/foo/%22%21$&%27()()*%2B,;=%7C%22") := '(foo "!$&'()()*+,;=|")

  "composite parameters"
  (encode `(hyperfiddle.blog/post [:user/sub "google-oauth2|1234"] #{"events" "news"}))
  := "/hyperfiddle.blog%2Fpost/(:user%2Fsub%20%22google-oauth2%7C1234%22)/%23%7B%22news%22%20%22events%22%7D"

  (decode "/hyperfiddle.blog%2Fpost/(:user%2Fsub,%22google-oauth2%7C1234%22)/%23%7B%22news%22,%22events%22%7D")
  := `(hyperfiddle.blog/post [:user/sub "google-oauth2|1234"] #{"events" "news"})

  (decode "/bar/%22qwerzxcv%22") := '(bar "qwerzxcv")
  (decode "/bar/((%3Fe,:foo,%3Fa))/%22qwerzxcv%22#blah") := '(bar [[?e :foo ?a]] "qwerzxcv"))

(tests
  "sexpr bijection check (egality only, can lose collection type)"
  (def home `(user/hello))
  (def x `(user/hello :a 1 "alice"))
  ((comp decode encode) x) := x
  ((comp #(decode % home) #(encode % home)) home) := home

  ;(defmacro check [x] `(let [x# ~x] ((comp decode encode) x#) := x#)) -- RCF bug
  (defn check [x] (= x ((comp decode encode) x)))

  (check x) := true
  (check `(hyperblog/post)) := true
  (check `(hyperblog/post 17592186045826)) := true
  (check `(hyperblog/post :user/hello-world)) := true
  (check `(hyperfiddle.blog/post [:user/sub "google-oauth2|1234"] #{"events" "news"})) := true
  ;(check "/foo/'!$&'()()*+,;=%7C'") -- what's this?
  )

; malformed:
;[:user/sub "google-oauth2|116635422485042503270"]
;(decode "/") [nil [nil]]
;(decode "/garbagasdf..23425649=//e")
;(decode "/asdf/asdf/asdf?asdf?asdf?sadf")
