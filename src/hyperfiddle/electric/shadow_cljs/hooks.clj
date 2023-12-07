(ns hyperfiddle.electric.shadow-cljs.hooks
  (:require [hyperfiddle.electric :as-alias e]
            [hyperfiddle.electric.impl.lang :as lang]
            [clojure.string :as str]
            [hyperfiddle.electric.impl.expand :as expand]))

(let [!first-run? (volatile! true)]     ; first run is noop
  (defn reload-clj
    "When any Electric def is changed, recompile it in both Clojure and ClojureScript
(because the expression may contain e/client and/or e/server). Takes care to prevent
double reloads (i.e. from :require-macros)."
    {:shadow.build/stage :compile-finish} [build-state]
    (if @!first-run?
      (vreset! !first-run? false)
      (when (= :dev (:shadow.build/mode build-state))
        (let [compiled-keys (-> build-state :shadow.build/build-info :compiled)
              cljc-infos (eduction (filter (fn [[_ f]] (str/ends-with? f ".cljc")))
                           (map #(get (:sources build-state) %)) compiled-keys)]
          (doseq [{ns-sym :ns, macro-requires :macro-requires} cljc-infos]
            (when (and (not (get macro-requires ns-sym)) (-> ns-sym find-ns meta ::lang/has-edef?))
              (prn ::reloading ns-sym)
              (swap! expand/!cljs-ns-cache dissoc ns-sym)
              (require ns-sym :reload))))))
    build-state))

(defmacro when-class-available
  "Expands to `body` in a java class named by `class-sym` is available on the classpath at compile time.
   Expands to nil otherwise."
  [class-sym & body]
  (when (try (Class/forName (name class-sym))
             (catch ClassNotFoundException _ false))
    `(do ~@body)))

(defn jackson-version []
  (when-class-available com.fasterxml.jackson.core.json.PackageVersion
    (bean com.fasterxml.jackson.core.json.PackageVersion/VERSION)))

(defn is-jackson-limiting-string-size?
  "Read Jackson’s version and return:
  - ::no-limit if version is older than 2.15,
  - ::limited if version is 2.15 or 2.15.1,
  - ::configurable if version is at least 2.15.2,
  - ::unavailable if Jackson is not on the classpath." []
  (if-let [version (jackson-version)]
    (let [{:keys [majorVersion minorVersion patchLevel]} version]
      (cond
        (and (<= majorVersion 2) (< minorVersion 15))                   ::no-limit     ; < 2.15, no string size limit, we are good.
        (and (<= majorVersion 2) (<= minorVersion 15) (< 2 patchLevel)) ::limited      ; < 2.15.2, with hard-coded limits (non-configurable)
        :else                                                           ::configurable ; > 2.15.2, limits can be configured, we are good.
        ))
    ::unavailable))


(let [!jackson-string-size-limit-setting (atom nil)]
  (defn allow-large-shadow-cache
    "Allows large shadow-cljs cache files.

  This Shadow hook will expand Jackson’s default max string size by `factor`.

  Shadow-cljs caches compiled cljs namespaces into transit-encoded json files.
  Electric e/def(n) can expand to a lot of clojure/script code. The Hyperfiddle
  team is working on improving the generated code size. In the meantime, we argue
  large code size is not an actual issue because:
  - server-side code size is not a constraint for the use case of Electric
  (long-living web apps),
  - client-side code size is efficiently optimized away by the Google Closure
  advanced compilation mode (e.g. shadow release),
  - gzip compression outshines most code size optimizations.

  However, code size matters in dev mode (e.g. shadow watch). Shadow will cache
  compiled cljs namespaces in transit-JSON format. Shadow caches the entire
  compiled js file as a JSON string. Transit reads and writes JSON with the
  Jackson library.

  Since Jackson 2.15, strings larger than 5Mb were rejected¹. The community
  quickly asked for this limit to be raised to 20Mb² and to be configurable³.
  These changes were released in Jackson 2.15.2.

  As of December 2023, Transit-java (and so Transit-clj) does not set a max string
  size and relies on defaults. We can therefore safely alter defaults for the 
  extent of a shadow compilation.

  For each compilation (dev and release), this shadow hook will:
  - Before compilation:
    - capture the current Jackson max string size limit,
    - set the limit to `default × factor`.
  - After compilation:
    - restore the captured max string size limit.


  ¹ https://github.com/FasterXML/jackson/wiki/Jackson-Release-2.15#processing-limits
  ² https://github.com/FasterXML/jackson-core/issues/1014
  ³ https://github.com/FasterXML/jackson-core/pull/1019
  "
    {:shadow.build/stages #{:compile-prepare :flush}}
    ([build-state]
     (allow-large-shadow-cache build-state 1))
    ([build-state factor]
     (assert (nat-int? factor) "Cache size factor should be a positive integer.")
     (when-class-available com.fasterxml.jackson.core.json.PackageVersion
         (case (:shadow.build/stage build-state)
           :compile-prepare (case (is-jackson-limiting-string-size?)
                              ::unavailable nil
                              ::no-limit    nil
                              ::limited     (println "Your Shadow-cljs setup is too old or one of you dependencies pulled in a version of Jackson older than 2.15.2 ."
                                              "Electric Clojure hot code reload and recompilation might be slow due to Shadow failing to read large cache files."
                                              "Update your dependencies to get Jackson >= 2.15.2 for Electric Clojure to benefit from Shadow-cljs caching."
                                              "See `hyperfiddle.electric.shadow-cljs.hooks/allow-large-shadow-cache`") ; TODO permalink to github source
                              ::configurable
                              (swap! !jackson-string-size-limit-setting
                                (fn [current-default-setting]
                                  (if (some? current-default-setting)
                                    (do (println "Electric Clojure detected two concurrent Shadow-cljs compilations. Client and Server programs might be misaligned.")
                                        current-default-setting) ; another compilation is in progress, don’t race with it.
                                    (let [current-defaults                                          (com.fasterxml.jackson.core.StreamReadConstraints/defaults)
                                          {:keys [maxNestingDepth maxNumberLength maxStringLength]} (bean current-defaults)]
                                      #_(println "Snapshoting default jackson limits" (dissoc (bean current-defaults) :class))
                                      (com.fasterxml.jackson.core.StreamReadConstraints/overrideDefaultStreamReadConstraints
                                        (.build (doto (com.fasterxml.jackson.core.StreamReadConstraints/builder)
                                                  (.maxStringLength (* factor maxStringLength))
                                                  (.maxNumberLength maxNumberLength)
                                                  (.maxNestingDepth maxNestingDepth))))
                                      current-defaults)))))
           :flush           (swap! !jackson-string-size-limit-setting
                              (fn [current-default-setting]
                                (when current-default-setting
                                  #_(println "restoring default jackson settings " (dissoc (bean current-default-setting) :class))
                                  (com.fasterxml.jackson.core.StreamReadConstraints/overrideDefaultStreamReadConstraints current-default-setting)
                                  nil)))
           nil))
     build-state)))
