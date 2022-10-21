(ns dustin.y2020.monad-scope2
  (:require
    [minitest :refer [tests]]))


(defn pure [a] (fn [scope] [scope a]))

(defn runScope [m scope] (m scope))

(tests
  (runScope (pure 42) {})
  := [{} 42]
  )

(defn bind [ma mf]                                          ; https://kseo.github.io/posts/2017-01-21-writer-monad.html
  (fn [scope]
    (let [[delta-scope a] (ma scope)]
      (runScope (mf a) (merge scope delta-scope #_{'% a})))))     ; flatten, and log-a is unwound after

(tests
  (def ma (pure 42))

  (runScope (bind ma pure) {})
  := [{} 42]

  (runScope (bind ma (fn [a] (fn [scope] [{'a a} (inc a)]))) {})
  := '[{a 42} 43]

  (runScope (bind ma (fn [a] (fn [scope] [scope (inc (get scope 'z))]))) {'z 0})
  := '[{z 0} 1]

  (runScope (bind ma (fn [a] (fn [scope] [{'z 1} (inc (get scope 'z))]))) {'z 0})
  := '[{z 1} 1]

  ;true := false

  )

(defn hf-edge->sym [edge]
  (if (keyword? edge)
    (symbol edge)
    ; todo need src
    #_edge))

;(defn my-resolve [s]
;  (if (symbol? s)
;    (clojure.core/resolve s)
;    s))
;
;(tests
;  (my-resolve :foo) := :foo
;  (my-resolve 'inc) := #'inc
;  )

;(defn fmap [f ma]
;  (let [X (fn [scope]
;            (let [[delta-scope a] (ma scope)
;                  b ((my-resolve f) a)]
;              [(merge scope delta-scope) b]))]
;    (fn [scope]
;
;
;      (let [[_ c] (bind X (fn [scope']
;                            (let [[_ c] ...]
;                              c)))]
;        [scope c])
;
;
;
;      (let [[delta-scope c] (runScope X scope)]
;        [scope c]))))


;(tests
;  (runScope (fmap 'inc (pure 1)) {'a 99})
;  := [{a 99, inc 2} 2]
;
;  (def a {:foo 1})
;  (runScope (fmap 'inc (fmap :foo (pure a))) {})
;  := [{} 2]
;
;  )

(defn fmap [f & mas]
  (fn [scope]
    (let [as (->> mas
               (map (fn [ma]
                      (let [[scope' a] (runScope ma scope)]
                        a))))
          b (apply f as)]
      [(merge scope {'% b} (if-let [s (hf-edge->sym f)] {s b}))
       b])))
; But, dissoc the scope on the way out

(defn fmap2 [f & mas]
  (fn [scope]
    (let [as (->> mas
               (map (fn [ma]
                      (let [[scope' a] (runScope ma scope)]
                        a))))
          b (apply f as)]
      [(merge scope {'% b} (if-let [s (hf-edge->sym f)] {s b}))
       b])))

(tests
  (runScope (fmap inc (pure 1)) {})
  := ['{% 2} 2]

  (runScope (fmap :foo (pure {:foo 1})) {})
  := ['{% 1, foo 1} 1]
  (runScope (fmap + (pure 1) (pure 2)) {})
  := ['{% 3} 3]
  (runScope (fmap vector (pure 1) (pure 2)) {})
  := ['{% [1 2]} [1 2]]
  (runScope (fmap inc ma) {'x 99})
  := ['{x 99, % 43} 43]
  (runScope (fmap inc (fmap inc ma)) {'x 99})
  := ['{x 99, % 44} 44]
  )

(defn traverse "[mv] -> m [v]" [mvs]
  (when mvs
    (apply fmap vector mvs)))

(tests
  (runScope (traverse []) {})
  := ['{% []} []]
  (runScope (traverse [(pure 1) (pure 2)]) {})
  := ['{% [1 2]} [1 2]]
  )
