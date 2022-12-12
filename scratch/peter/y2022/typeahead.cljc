(ns peter.y2022.typeahead
  (:refer-clojure :exclude [class])
  (:require
   #?(:cljs [hyperfiddle.ui.test :as uit])
   [hyperfiddle.photon :as p]
   [hyperfiddle.photon-dom :as dom]
   [hyperfiddle.photon-ui2 :as ui2]
   [hyperfiddle.rcf :as rcf :refer [tests tap %]]
   [clojure.string :as str])
  #?(:cljs (:require-macros peter.y2022.typeahead))
  (:import [missionary Cancelled]
           [hyperfiddle.photon Pending]))

(def class-ns "hf-typeahead-")
(defn class [& args] (apply str class-ns args))
#?(:cljs (defn elem [typ] (.createElement js/document typ)))
(p/def event)
(defmacro on [evt & body] `(when-some [e# (dom/Event. ~evt false)] (binding [event e#] ~@body)))
(defmacro with
  "Same as dom/with but does the actual mounting (appendChild)"
  [dom-node & body]
  `(let [node# ~dom-node]
     (.appendChild dom/node node#)
     (binding [dom/node node#] (new (p/hook dom/hook dom/node (p/fn [] dom/keepalive ~@body))))))

(defmacro container [!picking? & body]
  `(with (elem "div")
     (dom/props {:class [(class "container")]})
     ;; `some? event` forces the side effect to run, otherwise work skipped!
     (on "focusin" (reset! ~!picking? (some? event)))
     ~@body))

(defmacro input [input-elem & body]
  `(with ~input-elem
     (dom/props {:type "text" :class [(class "input")]})
     ~@body))

(defmacro close-on-click-unless-clicked-input [input-elem !picking?]
  `(binding [dom/node js/document]
     (on "click" (when-not (= ~input-elem (.. event -target)) (reset! ~!picking? false)))))

(p/def select!)

(defmacro typeahead-item [item & body]
  `(let [item# ~item]
     (with (elem "div")
       (dom/props {:style {:display "contents"}, :class [(class "picklist-item")]})
       (on "click" (.preventDefault event) (select! item#))
       ~@body)))

(defmacro typeahead [PickList ItemToText & body]
  (let [!picking? (gensym "!picking?"), input-elem (gensym "input-elem")]
    `(let [~input-elem (elem "input")
           !ret# (atom nil), ret# (p/watch !ret#)
           ~!picking? (atom false), picking?# (p/watch ~!picking?)]
       (container ~!picking?
         (input ~input-elem ~@body)
         (when picking?#
           (close-on-click-unless-clicked-input ~input-elem ~!picking?)
           (binding [select! #(reset! !ret# %)]
             (new ~PickList (binding [dom/node ~input-elem] (new ui2/Value))))))
       (let [txt# (new ~ItemToText ret#)] (set! (.-value ~input-elem) txt#))
       ret#)))

;; thoughts
;;
;; PickList returns a full set of results, eagerly. If we paginate it only needs to return a page's worth

;; tests

#?(:cljs (defn picklist-items [] (into [] (.querySelectorAll js/document (str "." class-ns "picklist-item")))))
#?(:cljs (defn picklist-item [s] (first (filter #(= s (.-innerText %)) (picklist-items)))))

(defmacro assert-visible-items-are [items]
  `(let [items# ~items, elems# (picklist-items)]
     (count items#) := (count elems#)
     (doseq [[item# text#] (map vector elems# ~items)]
       (.-innerText item#) := text#)))

#?(:cljs
   (tests
     (def -data {:alice "Alice B", :bob "Bob C", :charlie "Charlie D", :derek "Derek B"})
     (defn q [search] (into [] (comp (filter #(or (empty? search) (str/includes? (second %) search))) (map first)) -data))
     (def tphd (atom :missing))
     (def discard (p/run (try (binding [dom/node (dom/by-id "root")]
                                (tap (typeahead (p/fn [search]
                                                  (tap [:picklist-refresh search])
                                                  (p/for [e (q search)]
                                                    (typeahead-item (tap e)
                                                      (with (elem "div") (dom/text (get -data e))))))
                                       (p/fn [e] (get -data e))
                                       (reset! tphd dom/node))))
                              (catch Pending _)
                              (catch Cancelled _)
                              (catch :default e (prn e)))))
     % := nil
     (uit/focus @tphd)
     % := [:picklist-refresh ""]
     (hash-set % % % %) := #{:alice :bob :charlie :derek}
     (assert-visible-items-are (vals -data))
     (uit/set-value! @tphd "C")
     % := [:picklist-refresh "C"]
     ;; (hash-set % %) := #{:bob :charlie} ; doesn't happen, p/for
     (assert-visible-items-are [(:bob -data) (:charlie -data)])
     (uit/click (picklist-item "Charlie D"))
     % := :charlie
     (count (picklist-items)) := 0
     (.-value @tphd) := "Charlie D"
     ;; (uit/focus @tphd)
     ;; (uit/set-value! @tphd "")
     ;; (uit/press @tphd "Enter")
     ;; % := :alice
     ;; (.-value @tphd) := "Alice B"
     (discard)
     ))
