(ns hyperfiddle.hfql2.ui
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.spec :as spec]
            [hyperfiddle.logger :as log]
            [hyperfiddle.photon-ui :as ui])
  #?(:cljs (:require-macros [hyperfiddle.hfql2.ui])))

(p/defn Default-options-renderer [V props]
  (when-let [options (::hf/options props)]
    (p/client
      (let [group-id (random-uuid)] ; radio group unique id. Should it be the HFQL context path?
        (dom/fieldset
          (dom/legend (dom/text "Options"))
          (dom/table
            (p/server
              (let [labelf  (::hf/option-label props)
                    current (hf/Data. V)]
                (p/for [option (new options)]
                  (let [label     (if labelf (labelf option) option)
                        selected? (= current option)]
                    (p/client
                      (dom/tr
                        (dom/td (ui/checkbox {::dom/type :radio
                                              ::dom/name group-id
                                              ::dom/id   (hash label)
                                              ::ui/value selected?}))
                        (dom/td (dom/label {::dom/for (hash label)} (dom/text label)))))))))))))))

(p/defn Default-renderer [V props]
  (let [edn (hf/Data. V)]
    (p/client
      (dom/pre
        (dom/text edn)))))

(p/defn Form-renderer-impl [V props]
  (let [data (V.)
        ks (keys data)]
    (p/client
      (dom/form
        (p/for [k ks]
          (dom/label (dom/text k))
          (p/server (new (get data k))))))
    (Default-options-renderer. V props)))

(p/def Form-renderer Form-renderer-impl)

(p/defn Row-renderer "A row is a transposed form" [V props]
  (binding [Form-renderer Form-renderer-impl]
    (let [row     (V.)
          columns (::hf/columns props)]
      (p/client
        (dom/tr
          (p/for [col columns]
            (dom/td (p/server (new (get row col))))))))))

(p/defn Table-renderer [V props]
  (let [columns (::hf/columns props)]
    (p/client
      (dom/table
        (dom/thead
          (dom/tr
            (p/for [col columns]
              (dom/th (pr-str col))  ; TODO attr info on hover
              )))
        (dom/tbody
          (p/server
            (binding [Form-renderer Row-renderer]
              (p/for [row (V.)]
                (row.)))))))))

(def input-type {:hyperfiddle.spec.type/symbol  "text"
                 :hyperfiddle.spec.type/uuid    "text"
                 :hyperfiddle.spec.type/uri     "text"
                 :hyperfiddle.spec.type/instant "date"
                 :hyperfiddle.spec.type/boolean "checkbox"
                 :hyperfiddle.spec.type/string  "text"
                 :hyperfiddle.spec.type/bigdec  "text"
                 :hyperfiddle.spec.type/keyword "text"
                 :hyperfiddle.spec.type/ref     "text"
                 :hyperfiddle.spec.type/float   "number"
                 :hyperfiddle.spec.type/double  "number"
                 :hyperfiddle.spec.type/long    "number"})

(p/defn Input-renderer [V props]
  (let [v          (V.)
        value-type (::value-type props)]
    (p/client
      (ui/input {::ui/value v
                 ::ui/type  (input-type value-type "text")}))))

(defn spec-value-type [attr] (spec/type-of attr)) ;; TODO extract spec for quoted sexpr, TODO support args
(defn schema-attr [db ?a]
  (log/debug "Query DB schema for attr " ?a)
  #?(:clj (condp = (type db)
            datascript.db.DB (get (:schema db) ?a))))

(defn schema-value-type [db a]
  (let [attr (schema-attr db a)]
    (spec/valueType->type (or (:db/valueType attr) (:hf/valueType attr))))) ; datascript rejects valueType other than ref.

(p/defn Spec-renderer [V props]
  (let [attr       (::hf/attribute props)
        value-type (or (spec-value-type attr) (schema-value-type hf/*$* attr))]
    (case value-type
      :hyperfiddle.spec.type/string (Input-renderer. V (assoc props ::value-type value-type))
      (Default-renderer. V props))))

(p/defn Render [V props]
  (if hf/bypass-renderer
    (hf/Join-all. (V.))
    (if-let [Renderer (::hf/render props)]
      (Renderer. V props)
      (let [Renderer (case (::hf/render-as props)
                       ::hf/form  Form-renderer
                       ::hf/table Table-renderer
                       ::hf/field Spec-renderer
                       Default-renderer)]
        (Renderer. V props)))))
