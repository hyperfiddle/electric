(ns contrib.sexpr-router
  (:require [clojure.string :as string]
            contrib.ednish
            [hyperfiddle.rcf :refer [tests]]))

(defn encode [route & [home-route]]
  (let [[f & args] route]
    (condp = route
      home-route "/"
      (str "/" (contrib.ednish/encode-uri f)
           "/" (some->> args
                        (map contrib.ednish/encode-uri)
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
           (map contrib.ednish/decode-uri)))
    (throw (ex-info (str "Invalid url: " s) {::s s}))))

(tests
  "sexpr routes"
  (encode `(user/hello)) := "/user!hello/"
  (decode "/user!hello/") := `(user/hello)

  (encode `(user/hello :a 1 "alice")) := "/user!hello/:a/1/'alice'"
  (decode "/user!hello/:a/1/'alice'") := `(user/hello :a 1 "alice")

  "pass a home-route"
  (def home `(user/hello :default))
  (encode `(user/hello :default) home) := "/"
  (decode "/" home) := `(user/hello :default)
  (encode `(user/hello :a 1 "alice") home) := "/user!hello/:a/1/'alice'"
  (decode "/user!hello/:a/1/'alice'" home) := `(user/hello :a 1 "alice")

  (encode `(user/hello) home) := "/user!hello/"
  (decode "/user!hello/" home) := `(user/hello)

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
  (encode '(foo "!$&'[]()*+,;=|")) := "/foo/'!$&'()()*+,;=%7C'"
  ;(decode "/foo/'!$&'()()*+,;=%7C'") := '(foo "!$&'[]()*+,;=|") -- todo why broken?

  "composite parameters"
  (encode `(hyperfiddle.blog/post [:user/sub "google-oauth2|1234"] #{"events" "news"}))
  := "/hyperfiddle.blog!post/(:user!sub,'google-oauth2%7C1234')/~%7B'news','events'%7D"

  (decode "/hyperfiddle.blog!post/(:user!sub,'google-oauth2%7C1234')/~%7B'news','events'%7D")
  := `(hyperfiddle.blog/post [:user/sub "google-oauth2|1234"] #{"events" "news"})

  (decode "/bar/'qwerzxcv'") := '(bar "qwerzxcv")
  (decode "/bar/((%3Fe,:foo,%3Fa))/'qwerzxcv'#blah") := '(bar [[?e :foo ?a]] "qwerzxcv"))

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
