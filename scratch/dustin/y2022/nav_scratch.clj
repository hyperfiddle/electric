(ns dustin.y2022.nav-scratch
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap % with]]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [missionary.core :as m]
            [clojure.datafy :refer [datafy]]
            clojure.core.protocols
            clojure.repl))

(hyperfiddle.rcf/enable!)


(comment
  (as-> (datafy *ns*) % (nav % :publics (:publics %)))

  (as-> (datafy (type 1)) % (nav % :members (:members %)))

  (as-> java.lang.Long % (datafy %)
        (nav % :members (:members %)) (datafy %)
        (get % 'BYTES) (datafy %)
        (get % 0) (datafy %)
        #_(:declaring-class %)
        (nav % :declaring-class (:declaring-class %)) (datafy %))
  )

(defn sortmap [m]
  (into (sorted-map) m))

(defn- with-var-nav [v]
  (with-meta
    v
    {'clojure.core.protocols/nav
     (fn [_ k v]
       (if (var? v)
         (let [sym (.toSymbol v)
               ns (namespace sym)
               n (name sym)]
           {:source (clojure.repl/source-fn sym)
            :doc (with-out-str (@#'clojure.repl/print-doc (meta v)))})
         v))}))

(extend-protocol clojure.core.protocols/Datafiable
  clojure.lang.Namespace
  (datafy [n]
    (let [m {:name (.getName n)
             :publics (-> n ns-publics sortmap)
             :imports (-> n ns-imports sortmap)
             :interns (-> n ns-interns sortmap)}]
      (with-meta m (merge (meta n)
                          {'clojure.core.protocols/nav
                           (fn [_ k v]
                             (case k
                               :publics (with-var-nav v) ; datafy
                               :interns (with-var-nav v)
                               v))})))))


(tests
  (datafy #'sortmap)
  (meta (datafy #'clojure.core/assoc))
  (datafy (find-ns 'clojure.core))
  (def m (reflect (find-ns 'clojure.core)))
  (keys m) := (:name :publics :imports :interns)
  (:name m) := 'clojure.core
  (count (:publics m))
  (count (:imports m))
  (count (:interns m))

  (meta m)

  ; nav just adds metadata
  (clojure.core.protocols/nav m :name (:name m)) := 'clojure.core
  (clojure.core.protocols/nav m :publics (:publics m)) := 'clojure.core

  )
