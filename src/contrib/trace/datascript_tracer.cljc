(ns contrib.trace.datascript-tracer
  (:require [clojure.math :as math]
            [contrib.trace :as ct]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.rcf :as rcf :refer [% tap tests with]]
            [contrib.crypt :as crypt]
            [datascript.core :as d])
  (:import [hyperfiddle.electric Pending])
  #?(:cljs (:require-macros contrib.trace.datascript-tracer)))

(e/def conn)
(e/def db)

(def ds-schema {:db/id        {:db/unique :db.unique/identity}
                ::ct/point-id {:db/unique :db.unique/identity}
                ::ct/trace-id {:db/unique :db.unique/identity}
                ::ct/traces   {:db/cardinality :db.cardinality/many}})

(defn- ds-listener [conn]
  (fn
    ([point] (d/transact! conn [point]))
    ([->point-id trace] (d/transact! conn [{::ct/point-id ->point-id ::ct/traces (::ct/trace-id trace)}
                                           (cond-> trace (nil? (::ct/v trace)) (assoc ::ct/v ::nil))]))))
(defn de-nil [v] (if (= ::nil v) nil v))
(defn textify [v] (cond (nil? v) "nil"
                        (instance? Pending v) "⌛"
                        (instance? #?(:clj Throwable :cljs js/Error) v) (ex-message v)
                        :else v))

(defn default-point-id [id parent] (crypt/sha256-base64 [id parent]))
(defn ->default-trace-id []
  (let [!cache (atom {})]
    (fn [point-id _v]
      (let [nx (get (swap! !cache update point-id (fnil inc 0)) point-id)]
        (str point-id "-" nx)))))
(defn ms [_] #?(:clj (System/currentTimeMillis) :cljs (.now js/Date)))

(tests
  (def gen (->default-trace-id))
  (gen "x" nil) := "x-1"
  (gen "x" nil) := "x-2"
  (gen 123 nil) := "123-1"
  (gen 123 nil) := "123-2")

(defmacro with-defaults [& body]
  `(binding [ct/->trace-id (->default-trace-id), ct/stamp ms, ct/->point-id default-point-id
             conn (d/create-conn ds-schema)]
     (binding [db (e/debounce 50 (e/watch conn))]
       (ct/with-listener (ds-listener conn)
         ~@body))))

(tests "defaults work"
  (defn find-points-named [nm -db]
    (d/q '[:find (pull ?e [::ct/parent ::ct/traces]) . :in $ ?nm :where [?e ::ct/name ?nm]] -db nm))
  (defn find-traces [pt -db]
    (d/q '[:find (pull ?e [::ct/type ::ct/v]) :in $ [?tid ...] :where [?e ::ct/trace-id ?tid]] -db (::ct/traces pt)))
  (defn ->trace-time [trace-id -db] (::ct/stamp (d/entity -db [::ct/trace-id trace-id])))

  (def !conn (atom nil))
  (def !x (atom 3))
  (with (e/run (try (with-defaults (reset! !conn conn) (tap (ct/trace :+ (+ 2 (e/watch !x)))))
                    (catch #?(:clj Throwable :cljs :default) e (prn [(type e) (ex-message e)]))))
    % := 5
    (def -db (d/db @!conn))
    (def point (find-points-named :+ -db))
    (::ct/parent point) := ::ct/root
    (-> point ::ct/traces count) := 1
    (< (- (ms nil) (->trace-time (-> point ::ct/traces first) -db)) 10) := true
    (find-traces point -db) := [[{::ct/type ::ct/ok, ::ct/v 5}]]

    (swap! !x inc)
    % := 6
    (def -db (d/db @!conn))
    (def point (find-points-named :+ -db))
    (::ct/parent point) := ::ct/root
    (-> point ::ct/traces count) := 2
    (find-traces point -db) := [[{::ct/type ::ct/ok, ::ct/v 6}] [{::ct/type ::ct/ok, ::ct/v 5}]]
    ))

(defn ds-get [e k db] (get (d/entity db e) k))
(defn children [parent-id db] (vec (sort (d/q '[:find [?trace ...] :in $ ?parent :where [?trace ::ct/parent ?parent]] db parent-id))))
(defn ->traces [point db]
  (vec (sort (d/q '[:find [?e ...] :in $ ?point :where [?point ::ct/traces ?tid] [?e ::ct/trace-id ?tid]] db point))))
(defn ->latest [traces _db] (peek traces))

(tests
  (def -conn (atom nil))
  (with (e/run (with-defaults (reset! -conn conn) (tap (ct/trace :x (+ 1 2)))))
    % := 3
    (def -db (d/db @-conn))
    (children ::ct/root -db) := [1]
    (->traces 1 -db) := [2]))

;; rendering tips from Geoffrey:
;;
;; grid1                    grid2
;; ---------|-------------|-----------------
;; Name     | Value       | History
;; ---------|-------------|-----------------
;;
;; Render the value into these 2 grids
;; - grid2 can have a scrollbar now, fixing that issue
;; - grid1 gets a bit more tricky, we need to render into the cells correctly
;; - collapsing/expanding will also be a bit trickier
;;   - a simple boolean might be enough though, to unmount the rows or mark them display:none
;; - row height needs to be static and `=` in both grids
;; - values in time will be position:absolute or relative with an offset

(e/def RenderPoint)

(e/defn DSRenderPoint [point db depth]
  (dom/div (dom/style {:display "contents"})
    (dom/span (dom/style {:margin-left (str (* 8 depth) "px"), :border "1px solid gray", :height "30px"})
      (dom/text (ds-get point ::ct/name db)))
    (dom/span (dom/style {:border "1px solid gray", :height "30px"})
      (dom/text (-> point (->traces db) (->latest db) (ds-get ::ct/v db) de-nil textify)))
    (dom/div (dom/style {:display "contents"})
      (e/for-by #(ds-get % ::ct/point-id db) [child-point (children (ds-get point ::ct/point-id db) db)]
        (RenderPoint. child-point db (inc depth))))))

(defn next-measure-state [{:keys [status start]} id]
  (case status
    (:measured :idle) {:status :began :start id}
    (:began)          {:status :measured :start start :end id}))

;; TODO cleanup, 6 args.. Same with later fns
(e/defn DSRenderHistory [traces origin db pixel-secs !measure container-offset]
  (dom/div (dom/style {:height "30px", :position "relative", :border "1px solid gray"
                       :padding-right (str (+ container-offset 100) "px")})
    (e/for [trace traces]
      (let [stamp (ds-get trace ::ct/stamp db)
            ;; 200ms difference
            ;; 10px = 1sec = 1000ms
            ;; 10px/1000ms = offset/200ms
            ;; offset = 200ms*10px/1000ms = 2px
            offset (-> stamp (- origin) (* pixel-secs) (quot 1000))
            typ   (ds-get trace ::ct/type db)
            v     (de-nil (ds-get trace ::ct/v db))]
        (dom/span (dom/style {:position "absolute"
                              :left (str offset "px")
                              :background-color (case typ
                                                  ::ct/ok "#c5e8c5"
                                                  ::ct/err (if (instance? Pending v) "inherit" "#ffcaca"))})
          (dom/text (textify v))
          (dom/on! "click" (fn [_] (swap! !measure next-measure-state trace))))))))

(defn ->origin [db] (reduce min (ms nil) (d/q '[:find [?stamp ...] :where [_ ::ct/stamp ?stamp]] db)))

(e/def RenderTraces)

(e/defn DSRenderTraces [point db origin pixel-secs !measure offset]
  (DSRenderHistory. (->traces point db) origin db pixel-secs !measure offset)
  (e/for-by #(ds-get % ::ct/point-id db) [child-point (children (ds-get point ::ct/point-id db) db)]
    (RenderTraces. child-point db origin pixel-secs !measure offset)))

#?(:cljs (defn scroll-to-end [node _db] (set! (.-scrollLeft node) (.-scrollWidth node))))

(defn time-str [ms] (if (> ms 1000) (str (-> ms (/ 10) math/round (/ 100)) "s") (str ms "ms")))
(defn measure-distance [start end db] (abs (- (ds-get start ::ct/stamp db) (ds-get end ::ct/stamp db))))

(defn- calculate-history-container-offset [origin pixel-secs db]
  (-> (reduce max (d/q '[:find [?stamp ...] :where [_ ::ct/stamp ?stamp]] db))
    (- origin) (* pixel-secs) (quot 1000)))

(e/defn DatascriptTraceView []
  (binding [RenderPoint DSRenderPoint, RenderTraces DSRenderTraces]
    (let [!pixel-secs (atom 1000), pixel-secs (e/watch !pixel-secs)
          !measure (atom {:status :idle}), measure (e/watch !measure)
          !offset (atom 0), offset (e/watch !offset)
          origin (->origin db)]
      (dom/div (dom/props {:class "dstrace"})
        (dom/span (dom/text "time granularity: "))
        (ui/range pixel-secs (e/fn [v] (reset! !pixel-secs v))
          (dom/style {:display "inline-block", :width "200px"})
          (dom/props {:min 1, :max 10000}))
        (dom/div
          (dom/text "distance: "
            (when (= :measured (:status measure))
              (time-str (measure-distance (:start measure) (:end measure) db)))))
        (dom/div (dom/style {:display "flex"})
          (dom/div (dom/style {:display "inline-grid", :grid-template-columns "1fr 1fr", :min-width "400px"})
            (dom/strong (dom/text "Name")) (dom/strong (dom/text "Value"))
            (e/for [root-point (children ::ct/root db)]
              (RenderPoint. root-point db 0)))
          (dom/div (dom/style {:display "inline-grid", :overflow "scroll", :white-space "nowrap", :flex-grow 1})
            (dom/strong (dom/style {:height "22px"}) (dom/text "History"))
            (case (e/for [root-point (children ::ct/root db)]
                    (RenderTraces. root-point db origin pixel-secs !measure offset))
              (case (reset! !offset (calculate-history-container-offset origin pixel-secs db))
                (scroll-to-end dom/node [db pixel-secs])))))))))
