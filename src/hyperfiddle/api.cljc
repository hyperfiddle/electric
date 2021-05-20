(ns hyperfiddle.api)

(def ^:dynamic *$*)                                         ; available in cljs for HFQL datascript tests


;;;;;;;;;;;;;;;;;;;;
;; Semantic Types ;;
;;;;;;;;;;;;;;;;;;;;

(deftype Link [href value]
  Object
  (toString [this]
    (str "#hyperfiddle.api.Link " {:href href, :value value}))
  (equals [this other]
    (and (= href (.href other))
         (= value (.value other)))))

(deftype Input [id value onChange]
  Object
  (toString [this]
    (str "#hyperfiddle.api.Input" {:id    id
                                   :value value}))
  (equals [this other]
    (= (.id this) (.id other))))

#?(:clj (defmethod print-method Link [^Link v w]
          (.write w (.toString v))))

#?(:clj (defmethod print-method Input [^Input v w]
          (.write w (.toString v))))

#?(:cljs (cljs.reader/register-tag-parser! 'hyperfiddle.api.Link (fn [{:keys [href value]}] (Link. href value))))

#?(:cljs (cljs.reader/register-tag-parser! 'hyperfiddle.api.Input (fn [{:keys [id value]}] (Input. id value nil))))

#?(:cljs (extend-protocol IPrintWithWriter
           Link
           (-pr-writer [this writer _]
             (write-all writer "#hyperfiddle.api.Link " (pr-str {:href  (.-href this)
                                                                 :value (.-value this)})))
           Input
           (-pr-writer [this writer _]
             (write-all writer "#hyperfiddle.api.Input " (pr-str {:id    (.-id this)
                                                                  :value (.-value this)})))))
