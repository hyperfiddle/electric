(ns hyperfiddle.electric.debug3
  ; depends on electric by data namespace, c-f "electric"
  (:require [clojure.string :as str]
            [contrib.data :as data]
            #?(:cljs [contrib.stacktrace :as st])))

(defn ->id []
  #?(:clj  (java.util.UUID/randomUUID)
     :cljs (random-uuid)))

(defonce ^{:doc "A random unique ID generated for each Electric runtime instance (browser tab, jvm). Used to identify origin of a transfered value."}
  PEER-ID
  ;; UUID v4 collision probability assumed insignificant for this use case
  (->id))

(defn normalize-async-stack-frame [stack-frame]
  (let [meta        (::meta stack-frame)
        dbg-in-meta (data/select-ns :hyperfiddle.electric.debug (::meta stack-frame))]
    (merge stack-frame dbg-in-meta {::meta (apply dissoc meta (keys dbg-in-meta))})))

(defn get-async-trace [exception] (::trace (ex-data exception)))

(defn locate-stack-frame [{::keys [origin type meta] :as frame}]
  (cond-> frame
    (and (not= PEER-ID origin)
      (not (#{:transfer :toggle} type))) (assoc ::remote? true)
    (:line meta) (assoc ::line (:line meta))
    (:file meta) (assoc ::file (:file meta))))

(def fail? '#{hyperfiddle.electric.impl.runtime/fail})

(defn file->ns [file]
  (when file
    (when-let [match (second (re-find #"/?(.*)\..*$" (str file)))]
      (str/replace match #"/" "."))))

(defn js-stack-frame-name [{::keys [name ns file] :as _stack-frame}]
  (str (when (or ns file) (str (or ns (file->ns file)) ".")) name))

(defn js-stack-frame-location [{::keys [ns file line column]}]
  (when (or ns file)
    #?(:clj (str "(" (or ns file) (when line (str ":" line)) ")")
       :cljs (str #_(get-running-js-script-location) #_"/" (or ns file)
               (when line (str ":" line (when column (str ":" column))))))))

(defn render-canonical-js-stack-frame [frame]
  (str (js-stack-frame-name frame) " at " (js-stack-frame-location frame)))

;;; JVM stack frames

(defn path->file [path] (when path (last (str/split (str path) #"/"))))

(defn jvm-trace-element-class-name [{::keys [name ns file]}]
  (str (or ns (file->ns file)) "$" name))

(defn jvm-trace-element-file-name [{::keys [ns file]}]
  (cond
    file (path->file file)
    ns   (-> (str/replace (str ns) #"-" "_")
             (str/replace #"\." "/")
             (path->file))
    :else ""))

#?(:clj
   (defn render-jvm-stack-frame [{::keys [name line] :as stack-frame}]
     (StackTraceElement. (jvm-trace-element-class-name stack-frame) ;; canonicalize ns and filename to a_b.c$d
       (str name) (jvm-trace-element-file-name stack-frame) (or line 0))))

;;; Common interface

(defn render-stack-frame [stack-frame]
  #?(:clj (render-jvm-stack-frame stack-frame)
     :cljs (render-canonical-js-stack-frame stack-frame)))

(defn cleanup-async-stack-trace [stack-trace]
  (->> stack-trace
    (filter ::type)
    (map normalize-async-stack-frame)
    (map locate-stack-frame)
    (filter (fn [stack-frame]
              (and (::name stack-frame) ; only keep namepd and located frames, others are useless
                (or (::file stack-frame)
                  (::ns stack-frame)))))))

(defn render-async-stack-trace [trace]
  (let [trace-elements (->> trace
                         (cleanup-async-stack-trace)
                         (map render-stack-frame)
                         (dedupe))]
    #?(:clj trace-elements
       :cljs (not-empty (str/join "\n" trace-elements)))))

(defn async-stack-trace [exception]
  (render-async-stack-trace (get-async-trace exception)))

;;; -------

(defn unwrap [exception]
  (if (= ::trace (:hyperfiddle.electric/type (ex-data exception)))
    (or (ex-cause exception) exception)
    exception))


#?(:clj
   (defn stack-element-matches? [regex ^StackTraceElement elem]
     (re-matches regex (.getClassName elem))))

#?(:clj
   (defn keep-stack-elements-upto
     [upto-regex stack-trace-elements]
     (->> stack-trace-elements
       (reverse)
       (drop-while #(not (stack-element-matches? upto-regex %)))
       (reverse))))

#?(:clj
   (defn keep-relevent-stack-trace-elements
     ([stack-trace-elements] (keep-relevent-stack-trace-elements #"hyperfiddle.electric.impl.runtime.*" stack-trace-elements))
     ([upto-regex stack-trace-elements]
      (->> stack-trace-elements
        (keep-stack-elements-upto upto-regex)
        (reverse)
        (drop-while (fn [^StackTraceElement elem]
                      (and (#{"clojure.lang.AFn" "clojure.core$apply"} (.getClassName elem))
                        (#{"invoke" "applyTo" "invokeStatic" "applyToHelper"} (.getMethodName elem)))))
        (reverse)))))

#?(:clj
   (defn update-stack-trace!
     "Like `clojure.core/update` but updates an exception's stacktrace. Mutating the exception in place.
`f` receives a sequence of `java.lang.StackTraceElement` and any provided `args`.
`f` must return a sequence of `java.lang.StackTraceElement`s.
  Use case: tidy up (denoise) a stack trace by removing meaningless elements."
     [ex f & args]
     (->> (apply f (seq (.getStackTrace ex)) args)
       (into-array StackTraceElement)
       (.setStackTrace ex))
     ex))

#?(:clj
   (defn clean-jvm-stack-trace!
     ([ex] (clean-jvm-stack-trace! #"hyperfiddle.electric.impl.runtime.*" ex))
     ([upto-regex ex]
      (update-stack-trace! ex (partial keep-relevent-stack-trace-elements upto-regex)))))

;; (comment
;;   (clean-stack-trace! (try (apply inc nil) (catch Throwable t t))))

#?(:cljs
   (defn parse-canonical-js-stack-frame [js-stack-frame]
     (let [[name location] (-> js-stack-frame
                             (str/replace-first #"^>\s+" "")
                             (str/split #"\s+at\s+")
                             )]
       {::name name, ::location location})))

#?(:cljs
   (defn parse-js-stack-trace [js-stack-trace]
     (->> (st/canonicalize js-stack-trace)
       (str/split-lines)
       (map parse-canonical-js-stack-frame))))

#?(:cljs
   (defn serialize-canonical-js-stack-trace [parsed-js-stack-trace]
     (->> parsed-js-stack-trace
       (map (fn [{::keys [name location] :as _stack-frame}]
              (str name " at " location)))
       (str/join "\n"))))

#?(:cljs
   (defn cleanup-cljs-stack-trace [js-stack-trace] ; obtain a js-stack-trace with `(.-stack js-exception)`
     (when js-stack-trace
       (->> (parse-js-stack-trace js-stack-trace)
         (filter ::location)
         (take-while (fn [{::keys [location]}] (not (str/includes? location "hyperfiddle.electric.impl.runtime"))))
         (reverse)
         (drop-while (fn [{::keys [name]}] (str/starts-with? name "cljs.core.apply")))
         (reverse)
         (serialize-canonical-js-stack-trace)))))

(defn cleanup-js-stack-trace [js-stack-trace] ; obtain a js-stack-trace with `(.-stack js-exception)`
  (when js-stack-trace
    (->> (str/split-lines js-stack-trace)
      (take-while #(not (str/includes? % "hyperfiddle.electric.impl.runtime")))
      (reverse)
      (drop-while #(str/starts-with? % "cljs.core.apply"))
      (reverse)
      (str/join "\n"))))

;; #?(:cljs
;;    (defn dark-browser-theme? []
;;      (and (.-matchMedia js/window) (.-matches (.matchMedia js/window "(prefers-color-scheme: dark)")))))

(defn left-pad-stack-trace
  ([string]
   (left-pad-stack-trace 2 string))
  ([num-spaces string]
   (let [pad (str/join "" (repeat num-spaces " "))]
     (->> (str/split string #"\n")
       (map (fn [line] (str pad line)))
       (str/join "\n")))))

