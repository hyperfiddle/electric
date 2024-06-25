(ns contrib.trace-de
  (:require
   #?(:clj [contrib.triple-store :as ts])
   [clojure.math :as math]
   [contrib.debug]
   [contrib.str]
   [hyperfiddle.electric-de :as e :refer [$]]
   [hyperfiddle.electric-dom3 :as dom]))

(def current nil)
(declare !db db !measure, measure)
(let [c (atom {})]
  (defn ->trace-id [nm]
    [nm (-> (swap! c update nm (fnil inc 0)) (get nm))]))
(defn ->stamp [_] #?(:clj (System/currentTimeMillis) :cljs (.now js/Date)))
(let [!i (atom 0)] (defn ->id [_] (swap! !i inc)))

#?(:clj (defn save-trace [!db trace] (swap! !db ts/add (assoc trace :db/id (->id trace)))))

(defmacro trace
  ([nm form] `(trace ~nm identity ~form))
  ([nm ->pretty form]
   `(let [nm# (->trace-id ~nm)
          trace# {::id nm#, ::parent current}]
      (e/server (save-trace !db trace#))
      (let [v# (binding [current nm#] ~form)
            tracev# {::v-of nm#, ::stamp (->stamp v#), ::v v#, ::pretty-v (when (some? v#) (~->pretty v#))}]
        (e/server (save-trace !db tracev#))
        (e/server (e/on-unmount #(save-trace !db (assoc tracev# ::v nil, ::pretty-v "⛔"))))
        v#))))

(defn ->queue
  ([] #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))
  ([& args] (into (->queue) args)))

(defmacro with-defaults [& body]
  `(let [!db# (e/server (atom (ts/->ts))), m# (e/server (atom (->queue nil nil)))]
     (binding [!db !db#, db (e/server (e/watch !db#)), !measure m#, measure (e/server (e/watch m#))]
       ~@body)))

#?(:clj
   (defn get-latest-pretty-v [db id]
     (if-some [e (->> (ts/find db ::v-of id) reverse first)]
       (::pretty-v (ts/->node db e))
       "∅")))

(defn ->pretty [v] (if (nil? v) "␀" v))

(e/defn RenderPoint [e depth]
  (e/client
    (let [nd (e/server (ts/->node db e))]
      (dom/span
        (dom/props {:style {:margin-left (str (* 12 depth) "px")}})
        (dom/text (e/server (-> (::id nd) first name symbol))))
      (dom/span
        (dom/text (e/server (->pretty (get-latest-pretty-v db (::id nd))))))
      (e/cursor [c (e/server (e/diff-by identity (ts/find db ::parent (::id nd))))]
        ($ RenderPoint c (inc depth))))))

(def pixel-secs 1000)

(e/defn RenderPointHistory [vs-e origin]
  (dom/div
    (dom/props {:style {:position "relative"}})
    (e/cursor [ve (e/diff-by identity vs-e)]
      (let [nd (e/server (ts/->node db ve))
            ;; 200ms difference
            ;; 10px = 1sec = 1000ms
            ;; 10px/1000ms = offset/200ms
            ;; offset = 200ms*10px/1000ms = 2px
            offset (-> (e/server (::stamp nd)) (- origin) (* pixel-secs) (quot 1000))]
        (dom/span
          (dom/props {:style {:position "absolute", :left (str offset "px")}
                      #_ #_:title (e/server (contrib.str/pprint-str nd))})
          (dom/text (->pretty (e/server (::pretty-v nd))))
          (when-some [spend! ($ e/Token ($ dom/On "click"))]
            (spend! (e/server (swap! !measure (fn [m] (conj (pop m) ve)))))))))))

(e/defn RenderHistory [e origin]
  (let [id (e/server (::id (ts/->node db e)))]
    ($ RenderPointHistory (e/server (ts/find db ::v-of id)) origin)
    (e/cursor [ce (e/server (e/diff-by identity (ts/find db ::parent id)))]
      ($ RenderHistory ce origin))))

#?(:clj (defn ->origin [db] (->> db :ave ::stamp keys (reduce min))))

(e/defn Header [s] (dom/strong (dom/text s)))

(def grid-color "repeating-linear-gradient(to bottom, #fff 0, #fff 30px, #e5fff5 30px, #e5fff5 60px)")

(defn time-str [ms] (if (> ms 1000) (str (-> ms (/ 10) math/round (/ 100)) "s") (str ms "ms")))
#?(:clj (defn measure-distance [db [starte ende]]
          (when ende
            (abs (- (::stamp (ts/->node db starte)) (::stamp (ts/->node db ende)))))))

(e/defn TraceView []
  (e/client
    (dom/div
      (dom/props {:class "dstrace"})
      (dom/div (dom/text "Distance: " (e/server (some-> (measure-distance db measure) time-str))))
      (dom/div
        (dom/props {:style {:display "flex"}})
        (dom/div
          (dom/props {:style {:display "inline-grid", :grid-template-columns "1fr 1fr", :min-width "400px"
                              :background grid-color, :grid-auto-rows "30px"}})
          ($ Header "Name") ($ Header "Value")
          (e/cursor [root-e (e/server (e/diff-by identity (ts/find db ::parent nil)))]
            ($ RenderPoint root-e 0)))
        (dom/div
          (dom/props {:style {:display "inline-grid", :overflow "scroll", :white-space "nowrap", :flex-grow 1
                              :background grid-color, :grid-auto-rows "30px"}})
          ($ Header "History")
          (let [origin (e/server (->origin @!db))]
            (e/cursor [root-e (e/server (e/diff-by identity (ts/find db ::parent nil)))]
              ($ RenderHistory root-e origin))))))))
