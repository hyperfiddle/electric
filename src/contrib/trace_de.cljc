(ns contrib.trace-de
  (:require
   #?(:clj [contrib.triple-store :as ts])
   [clojure.math :as math]
   [contrib.debug]
   [contrib.str]
   [hyperfiddle.electric-de :as e :refer [$]]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:cljs [hyperfiddle.electric.impl.runtime-de :refer [Failure]])
   [missionary.core :as m])
  #?(:clj (:import [hyperfiddle.electric.impl.runtime_de Failure])))

(def current nil)
(declare !db db !measure measure !q q)
(let [c (atom {})]
  (defn ->trace-id [nm]
    [nm (-> (swap! c update nm (fnil inc 0)) (get nm))]))
(defn ->stamp ([] #?(:clj (System/currentTimeMillis) :cljs (.now js/Date))) ([_] (->stamp)))
(let [!i (atom 0)] (defn ->id [_] (swap! !i inc)))

#?(:clj
   (defn insert-trace [db trace]
     (if (::id trace)
       (cond-> db (not (ts/find db ::id (::id trace))) (ts/add (assoc trace :db/id (->id trace))))
       (ts/add db (assoc trace :db/id (->id trace))))))

(defn save-trace [!db trace] #?(:clj (swap! !db insert-trace trace)))
(defn push-trace [!q trace] #?(:cljs (swap! !q conj trace)))

(defn save-trace! [trace !db !q]
  (if (instance? Failure !db)
    (push-trace !q trace)
    (save-trace !db trace)))

(defn ->stable-trace-id [v] [v 0])

(letfn [(save [!db !q a b c]
          (save-trace! a !db !q)
          (save-trace! b !db !q)
          (save-trace! c !db !q))]
  (e/defn Trace
    ([nm F] ($ Trace nm identity F))
    ([nm ->pretty F] ($ Trace nm ->trace-id ->pretty F))
    ([nm ->trace-id ->pretty F]
     (let [nm (->trace-id nm)]
       (save-trace! {::id nm, ::parent current} !db !q)
       (save-trace! {::v ::mount, ::v-of nm, ::stamp (->stamp), ::pretty-v "🟢"} !db !q)
       (let [v (binding [current nm] ($ F))]
         (save-trace! {::v-of nm, ::stamp (->stamp v), ::v v, ::pretty-v (->pretty v)} !db !q)
         (e/on-unmount #(save-trace! {::v-of nm, ::stamp (->stamp), ::v ::unmount, ::pretty-v "🔴"} !db !q))
         v)))))

(defmacro trace
  ([nm form] `(trace ~nm identity ~form))
  ([nm ->pretty form] `(trace ~nm ->trace-id ~->pretty ~form))
  ([nm ->trace-id ->pretty form] `($ Trace ~nm ~->trace-id ~->pretty (e/fn [] ~form))))

(defn ->queue
  ([] #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))
  ([& args] (into (->queue) args)))

#?(:clj (defn save-traces [!db trace+] (swap! !db (fn [db] (reduce insert-trace db trace+)))))

(e/defn SendClientTraces [ms]
  (e/client
    (when-some [spend! ($ e/CyclicToken (seq q))]
      (case ($ e/Task (m/sleep ms))
        (let [[trace+] (swap-vals! !q (constantly []))]
          (spend! (e/server (save-traces !db trace+))))))))

(defmacro with-defaults [& body]
  `(let [!db# (e/server (atom (ts/->ts))), m# (e/server (atom (->queue nil nil))), q# (e/client (atom []))]
     (binding [!db !db#, db (e/server (e/watch !db#)), !measure m#, measure (e/server (e/watch m#))
               !q q#, q (e/client (e/watch q#))]
       ($ SendClientTraces 80)
       ~@body)))

#?(:clj (defn get-latest-pretty-v [db id]
          (->> (ts/find db ::v-of id) reverse first (ts/->node db) ::pretty-v)))

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
                      :title (e/server (contrib.str/pprint-str nd))})
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

(e/defn Throttle [ms v]
  (let [[v2 spend!] ($ e/StampedToken v)]
    (when spend! (spend! ($ e/Task (m/sleep ms))))
    (if spend! v2 v)))

(e/defn TraceView []
  ;; binding [db ($ Throttle 2000 db)]
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
