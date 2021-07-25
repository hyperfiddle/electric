(ns dustin.reactive-partial)

(def ^:dynamic *needle* "charlie")
(def !x (atom "bob"))
;; reactive-quote
(defnode bar [>x]
         (binding [*needle* "alice"]
           ~>x ~>x))
(defnode foo []
         (let [x ~(m/watch !x)]
           (bar (if (empty? x)
                  #'(prn [x *needle*])
                  #'(prn [*needle* x])))))
;; first-class nodes
(defnode bar [nf]
         (binding [*needle* "alice"]
           ($ nf) ($ nf)))
(defnode foo [then else]
         (let [x ~(m/watch !x)]
           (bar (if (empty? x)
                  (node [x] (prn [x *needle*]))
                  (node [x] (prn [*needle* x]))))))
;; ["bob" "charlie"] ;; initial design
;; ["bob" "alice"]   ;; proposition
;; we agree prn should happen twice (initial design is wrong for that matter)
;; not exposed
(def ^:dynamic %1)
(def ^:dynamic %2)
(def ^:dynamic %3)
(defmacro $ [f & args]
  `(binding [~@(interleave [%1 %2 %3] args)] (join ~f)))
(defmacro node [args & body]
  `(reactive-quote (let [~@(interleave args [%1 %2 %3])] ~@body)))
(defmacro partial [f & args]
  (let [syms (repeatedly gensym)]
    `(let [~@(interleave syms args)]
       (node
         ([x] ($ f ~@syms x))
         ([x y] ($ f ~@syms x y))))))