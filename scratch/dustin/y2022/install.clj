(ns dustin.y2022.install)

(comment
  (defn unquote [form] (if (and (seq? form) (= 'quote (first form))) (second form) form))
  #_(defn unquote [[x body :as form]] (if (and (seq? form) (= 'quote x)) body #_form))
  (defmacro install [& syms]
    (let [syms (map unquote syms)]
      (assert (every? qualified-symbol? syms))
      (into {} (map (juxt (partial list 'quote) identity)) syms))))