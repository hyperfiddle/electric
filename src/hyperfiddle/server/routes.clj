(ns hyperfiddle.server.routes)

(comment
  (require 'dustin.fiddle-pages))

;; TODO should either come from config or reflected from metas.
(def WHITELIST `#{dustin.fiddle-pages/page-submissions
                  dustin.fiddle-pages/page-submission-details})

;; TODO: use a real parser like edamame to be able to diagnose what's wrong with
;; a link. This is primitive routing, just parse sexp.
(defn route [http-request]
  (case (get-in http-request [:path-params :route])
    "dustin.fiddle-pages/page-submissions" `(dustin.fiddle-pages/page-submissions "")
    nil))

;;;;;;;;;;;
;; UTILS ;;
;;;;;;;;;;;

(defn- indices [xs] (into {} (map-indexed (fn [i x] [x i])) xs))

(defn sort-by-position
  "(sort-by-position identity [:a :b :c] [:c :b :a}]) => (:a :b :c)"
  ([positions xs]
   (sort-by-position identity positions xs))
  ([getter positions xs]
   (let [index-of (indices positions)]
     (sort (comparator (fn [x y]
                         (let [xi (index-of (getter x))
                               yi (index-of (getter y))]
                           (cond
                             (and xi yi) (< xi yi)
                             xi          true
                             yi          false))))
           xs))))


