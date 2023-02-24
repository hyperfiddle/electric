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

(defn error [debug-info ^Failure failure]
  (let [err (.-error failure)]
    (if (or (instance? Pending err) (instance? Cancelled err))
      failure
      (Failure. (add-stack-frame debug-info err)))))

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

(defn stack-trace [err]
  (some->> (::trace (ex-data err))
    (remove (fn [frame] (= {} (::name frame)))) ; (do a b) => ({} a b)
    (filter ::type)
    (reduce (fn [r frame]
              (if (string? frame)
                (conj r frame)
                (let [{::keys [origin type name params args macro scope meta]} (normalize-frame frame)]
                  (conj r
                    (into [(when (and (not= PEER-ID origin)
                                   (not (#{:transfer :toggle} type))
                                   "remote"))
                           (case scope
                             :lexical "lexically bound"
                             :dynamic "dynamically bound"
                             nil)]
                      (into
                        (case type
                          :apply         `["(" ~(case name
                                                  hyperfiddle.electric.impl.runtime/fail 'throw
                                                  name)
                                           ~@(map render-arg args) ")"]
                          :eval (let [{::keys [action target method args]} frame]
                                  (case action
                                    :field-access ["(" (str ".-" method) target ")"]
                                    :static-call  `["(" ~(str target "/" method) ~@(map render-arg (rest args)) ")"]
                                    :call         `["(" ~(str "." method) ~target ~@(map render-arg (rest args))")"]
                                    :fn-call      (if (some? name)
                                                    `["(" (clojure.core/fn ~name [~@params] ~'...) ~@(map render-arg args) ")"]
                                                    `["(" (clojure.core/fn [~@params] ~'...) ~@(map render-arg args) ")"])

                                    ["<unknown interop>" frame]))
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
                          `["<unknow frame>" ~(::ir/op frame)])
                        [(when macro (str "from macro " macro))
                         (some->> (:file meta) (str "in "))
                         (some->> (:line meta) (str "line "))]))))))
      [])
    (mapv (fn [frame] (if (string? frame) frame (str " in " (str/join " " (remove nil? frame))))))
    (str/join "\n")))

(defn unwrap [exception]
  (if (= ::trace (:hyperfiddle.electric/type (ex-data exception)))
    (ex-cause exception)
    exception))
