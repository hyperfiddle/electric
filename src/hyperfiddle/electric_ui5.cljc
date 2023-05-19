(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require clojure.edn
            contrib.str
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [contrib.debug :as dbg]))

(defmacro control [event-type parse unparse v V! setter & body]
  `(let [[state# v#] (e/for-event-pending-switch [e# (e/listen> dom/node ~event-type)]
                       (some->> (~parse e#) (new ~V!)))]
     #_(dom/style {:background-color (when (= ::e/pending state#) "yellow")})
     ; workaround "when-true" bug: extra outer when-some added to guard a nil from incorrectly sneaking through
     (when-some [v# (when (and (not (new dom/Focused?)) (#{::e/init ::e/ok} state#)) ~v)]
       (~setter dom/node (~unparse v#))) ; js coerce
     ~@body
     [state# v#]))

(e/def local?)
(def tempid? (some-fn nil? string?))

(defmacro entity [record EnsureEntity & body]
  `(let [[state# e#] (new ~EnsureEntity (:db/id ~record) ~record)]
     (case state# ::e/failed (.error js/console e#) nil)
     ;; mark entity local for downstream code
     (binding [local? (tempid? (:db/id ~record))]
       ~@body)))

(defmacro input [v V! & body]
  `(dom/input
     (let [[state# v#] (control "input" #(-> % .-target .-value) identity ~v ~V! dom/set-val)
           color# (if local? "blue" (case state# ::e/init "gray", ::e/ok "green", ::e/pending "yellow", ::e/failed "red"))]
       (dom/style {:border-width "2px", :border-style "solid", :border-color color#, :border-radius "0.2em"})
       (when local? (dom/props {:disabled true}))
       (case state# ::e/failed (.error js/console v#) nil)
       ~@body)))

(defmacro checkbox [v V! & body]
  `(dom/input (dom/props {:type "checkbox"})
     (let [[state# v#] (control "change" #(-> % .-target .-checked) identity ~v ~V! #(set! (.-checked %) %2))
           color# (if local? "blue" (case state# ::e/init "gray", ::e/ok "green", ::e/pending "yellow", ::e/failed "red"))]
       (dom/style {:outline-width "2px", :outline-style "solid", :outline-color color#})
       (when local? (dom/props {:disabled true}))
       (case state# ::e/failed (.error js/console v#) nil)
       ~@body)))
