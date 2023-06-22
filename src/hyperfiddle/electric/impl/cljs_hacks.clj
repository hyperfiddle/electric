(ns hyperfiddle.electric.impl.cljs-hacks)

;; As of 2023-06-22 electric compiles the program as a whole. This means all
;; code ends up in the entrypoint's namespace, e.g. user.cljs. When shadow-cljs
;; takes this cljs code and compiles it to javascript it fails to find goog
;; modules and produces warnings of undeclared-ns and undeclared-var. So if we
;; have a `(:require [goog.color])` in our ns and in electric code we call e.g.
;; `(goog.color/hslToHex 0.5 0.5 0.5)` then:
;;
;; `confirm-ns` here https://github.com/clojure/clojurescript/blob/219951678b16575ec29bb9b88047356cf1437aec/src/main/clojure/cljs/analyzer.cljc#L933
;;
;; - during electric compilation works because we're in the correct namespace
;; - during shadow compilation fails because we're in user.cljs
;;
;; This is not true for traditional cljs namespaces because they end up in the
;; compiler state here https://github.com/clojure/clojurescript/blob/219951678b16575ec29bb9b88047356cf1437aec/src/main/clojure/cljs/analyzer.cljc#L941
;; so the check succeeds.
;;
;; The clojurescript compiler has means to load the goog modules into this
;; compiler state as well. If one calls the cljs analyzer directly it ends up
;; there because there's a default pass called `ns-side-effects` defined here
;; https://github.com/clojure/clojurescript/blob/219951678b16575ec29bb9b88047356cf1437aec/src/main/clojure/cljs/analyzer.cljc#L4347
;; which eventually loads the goog modules into the compiler state.
;;
;; These passes are overriden in shadow-cljs here
;; https://github.com/thheller/shadow-cljs/blob/faab284fe45b04328639718583a7d70feb613d26/src/main/shadow/build/api.clj#L193
;; with no API to add passes. However even if it was possible adding
;; `ns-side-effects` pass fails to compile, the pass is probably not compatible
;; with some shadow changes.
;;
;; The code below fixes the warnings by monkey-patching the cljs analyzer's `ns`
;; parser to load goog modules into the compiler state. This fixes both the ns
;; and var warnings.
;;
;; It also populates the `:js-dependency-index` of the compiler state because
;; it's needed for loading the goog modules and shadow doesn't populate it.
;;
;; Note that we skip loading class-like modules such as `goog.math.Long` because
;; they already work and loading them actually breaks them (don't ask me why).

(def found-cljs-libs?                   ; run this ns only when ns's are on classpath
  (try (require
         '[cljs.analyzer :as ana]
         '[cljs.env :as env]
         '[cljs.externs :as externs]
         '[cljs.js-deps :as deps]
         '[shadow.build.compiler] ; rebinds ana/parse 'ns, so we need to load it
         ) true
       (catch java.io.FileNotFoundException _ false)))

(when found-cljs-libs?

  (defn lookup-goog-dep [cr goog-sym]
    (some-> (-> cr :js-dependency-index (get (name goog-sym)) :file)
      ;; (dbg/dbg :file)
      (externs/analyze-goog-file goog-sym)))

  (defn ensure-goog-ns-in-compiler-state! [!compiler goog-sym]
    ;; (prn :ensuring goog-sym)
    (when-not (contains? @!compiler :js-dependency-index)
      ;; (prn :adding-dependency-index)
      (swap! !compiler assoc :js-dependency-index (deps/js-dependency-index {})))
    (when (nil? (-> @!compiler ::ana/namespaces goog-sym :defs))
      ;; (prn goog-sym :not-in-compiler)
      (let [dep-info (lookup-goog-dep @!compiler goog-sym)]
        (when (:defs dep-info)
          (swap! !compiler update-in [::ana/namespaces goog-sym] merge dep-info)))))

  (defonce default-ns-parser (get-method ana/parse 'ns))

  (defmethod ana/parse 'ns [op env form nm opts]
    (let [{:keys [requires] :as ns-op} (default-ns-parser op env form nm opts)]
      (->> (vals requires)
        (filter #(re-find #"^goog\." (name %)))
        (remove #(re-find #"[A-Z]" (name %))) ; e.g. `goog.math.Long`
        (run! (partial ensure-goog-ns-in-compiler-state! env/*compiler*)))
      ns-op)))
