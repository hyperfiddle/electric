(ns hyperfiddle.electric.debug
  (:require #_[hyperfiddle.electric.impl.runtime :as-alias r]
            [clojure.string :as str]
            [contrib.data :as data]
            [hyperfiddle.electric.impl.ir :as-alias ir]
            [hyperfiddle.rcf :as rcf :refer [tests]])
  (:import (hyperfiddle.electric Failure Pending)
           (missionary Cancelled)
           #?(:clj (clojure.lang ExceptionInfo))
           (hyperfiddle.electric FailureInfo)))

(defn ->id []
  #?(:clj  (java.util.UUID/randomUUID)
     :cljs (random-uuid)))

(defonce ^{:doc "A random unique ID generated for each Electric runtime instance (browser tab, jvm). Used to identify origin of a transfered value."}
  PEER-ID
  ;; UUID v4 collision probability assumed insignificant for this use case
  (->id))

(defn ex-info*
  ([message data] (ex-info* message data nil))
  ([message data cause] (ex-info* message data (str (->id)) cause))
  ([message data id cause] (FailureInfo. message (assoc data :hyperfiddle.electric/type ::trace) id cause)))

(tests "2 traces with equal values are ="
  (let [cause #?(:clj (Throwable.) :cljs (js/Error.))]
    (ex-info* "" {} cause) := (ex-info* "" {} cause)
    nil))

(defn ex-id [ex] (.-id ^FailureInfo ex))

(defn add-stack-frame [frame ex]
  (let [frame (assoc frame ::origin PEER-ID)]
   (if (instance? FailureInfo ex)
    (ex-info* (ex-message ex) (update (ex-data ex) ::trace conj frame) (ex-id ex) (or (ex-cause ex) ex))
    (ex-info* (ex-message ex) {::trace [frame]} ex))))

(defn concat-stacks [ex1 ex2]
  (assert (instance? FailureInfo ex1))
  (assert (instance? FailureInfo ex2))
  (ex-info* (ex-message ex1) (update (ex-data ex1) ::trace into (::trace (ex-data ex2))) (or (ex-cause ex1) (ex-cause ex2) ex2)))

(defn error
  ([debug-info ^Failure failure]
   (error debug-info failure nil))
  ([debug-info ^Failure failure ^FailureInfo context]
   (let [err (.-error failure)]
     (if (or (instance? Pending err) (instance? Cancelled err))
       failure
       (Failure. (cond-> (add-stack-frame debug-info err)
                   (some? context) (concat-stacks context)))))))

(tests "rewrapping keeps same ID"
  (def ex (ex-info* "x" {}))
  (ex-id ex) := (ex-id (add-stack-frame {} ex)))

(defn render-arg [arg]
  (cond
    (string? arg) arg
    (ident? arg)  arg

    (or (instance? hyperfiddle.electric.Failure arg)
      #?(:clj (instance? Throwable arg)
         :cljs (instance? js/Error arg)))
    (symbol "<exception>")

    :else
    (binding [*print-level*  1
              *print-length* 4]
      (pr-str arg))))

(defn serializable-frame [frame]
  (if (::serializable frame)
    frame
    (-> (update frame ::args (partial mapv render-arg))
        (assoc ::serializable true))))

(defn serializable [map]
  (if (contains? map ::trace)
    (update map ::trace (partial mapv serializable-frame))
    map))

(defn normalize-frame [frame]
  (let [meta        (::meta frame)
        dbg-in-meta (data/select-ns :hyperfiddle.electric.debug (::meta frame))]
    (merge frame dbg-in-meta {::meta (dissoc meta dbg-in-meta)})))

(declare frames)

(defn expand-frame
  "Turn a stack frame into a sequence of lines to report"
  [frame]
  (let [{::keys [type name args]} frame]
    (case type
      :apply (case name
               ;; hyperfiddle.electric.impl.runtime/fail (concat (frames (second args)) [frame])
               [frame])
      [frame])))

(defn frames [err]
  (some->> (::trace (ex-data err))
           (remove (fn [frame] (= {} (::name frame)))) ; (do a b) => ({} a b)
           (filter ::type)
           (map normalize-frame)
           (mapcat expand-frame)))

(defn locate [frame]
  (let [{::keys [origin type meta] :as frame} (normalize-frame frame)]
    (cond-> frame
      (and (not= PEER-ID origin)
           (not (#{:transfer :toggle} type))) (assoc ::remote true)
      (:line meta) (assoc ::line (:line meta))
      (:file meta) (assoc ::file (:file meta)))))

(def fail? '#{hyperfiddle.electric.impl.runtime/fail})

(defn render-frame [frame]
  (let [{::keys [remote file line macro scope type name params args meta]} frame]
    (->> ["in"
          (when remote "remote")
          (when macro "macro")
          (case scope
            :lexical "lexically bound"
            :dynamic "dynamically bound"
            nil)

          (str/join " "
            (case type
              :apply (if (fail? name)
                       ["(throw" ~(render-arg (first args)) ")"]
                       `["(" ~name ~@(map render-arg args) ")"])
              :eval  (if (fail? (::fn frame))
                       `["(throw" ~(render-arg (first args)) ")"]
                       (let [{::keys [action target method args]} frame]
                         (case action
                           :field-access ["(" (str ".-" method) target ")"]
                           :static-call  `["(" ~(str target "/" method) ~@(map render-arg (rest args)) ")"]
                           :call         `["(" ~(str "." method) ~target ~@(map render-arg (rest args))")"]
                           :fn-call      (if (some? name)
                                           `["(" (clojure.core/fn ~name [~@params] ~'...) ~@(map render-arg args) ")"]
                                           `["(" (clojure.core/fn [~@params] ~'...) ~@(map render-arg args) ")"])
                           #_else (if-some [f (::fn frame)]
                                    `["(" ~f ~@(map render-arg args) ")"]
                                    ["<unknown interop>" frame]))))
              :reactive-fn   ["reactive" (if (some? name)
                                           `(~'fn ~name ~args ~'...)
                                           `(~'fn ~args ~'...))]
              :reactive-defn ["reactive" `(~'defn ~name ~args ~'...)]
              :try           ["(try ...)" ]
              :catch         [`(~'catch ~@args ~'...)]
              :finally       ["(finally ...)"]
              :case-clause   [`(~'case ~@args ~'...)]
              :case-default  ["case default branch"]
              :transfer      ["transfer to" (clojure.core/name name)]
              :toggle        ["transfer"]
              `["<unknow frame>" ~(::ir/op frame)]
              ))

          (when file (str "in " file))
          (when line (str "line " line))
          ]
      (remove nil?)
      (str/join " "))))

(defn render-stack-trace [err]
  (->> (frames err)
       (map locate)
       (map render-frame)
       (str/join "\n")))

(defn stack-trace [err] (render-stack-trace err))

(defn unwrap [exception]
  (if (= ::trace (:hyperfiddle.electric/type (ex-data exception)))
    (ex-cause exception)
    exception))
