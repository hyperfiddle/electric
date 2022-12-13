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
(defmacro on-busy [evt & body]
  `(p/with-cycle [busy# false]
     (when-some [e# (dom/Event. ~evt busy#)]
       (try (binding [event e#] ~@body false)
            (catch hyperfiddle.photon.Pending _e# true)))))
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

(defmacro close-on-click-unless-clicked-input [!picking?]
  `(let [input# dom/node]
     (binding [dom/node js/document]
       (on "click" (when-not (= input# (.. event -target)) (reset! ~!picking? false))))))

(p/def Select!)

(defmacro typeahead-item [item & body]
  `(let [item# ~item]
     (with (elem "div")
       (dom/props {:style {:display "contents"}, :class [(class "picklist-item")]})
       (on-busy "click" (.preventDefault event) (new Select! item#))
       ~@body)))

;; entC is typeahead's
;; we need to derive repC from that and pass that in as initial value
;; afterwards typing in the input field should refresh the rep atom
;; clicking on a div should reset rep atom to derived ent's rep as well

;; ent - an entity we're working with
;; rep - a string representation of the entity, that goes in the input
;; entC - controlled ent
;; repC - controlled rep
(defmacro typeahead [entC Picklist ItemToText & body]
  (let [!picking? (gensym "!picking?")]
    `(let [entC# ~entC, PL# ~Picklist, E->R# ~ItemToText
           ;; TODO atom gets rebuilt with each new `entC` value
           !rep# (atom (new E->R# entC#)), rep# (p/watch !rep#)
           !ent# (atom entC#), ent# (p/watch !ent#)
           ~!picking? (atom false), picking?# (p/watch ~!picking?)]
       (binding [Select! (p/fn [ent#] (let [rep# (new E->R# ent#)] (reset! !rep# rep#)) (reset! !ent# ent#))]
         (container ~!picking?
           (->> (ui2/input rep#
                  (dom/props {:class [(class "input")]})
                  (when picking?# (close-on-click-unless-clicked-input ~!picking?))
                  ~@body)
             (reset! !rep#))
           (when picking?# (new ~Picklist rep#))))
       ent#)))

;; thoughts
;;
;; Picklist returns a full set of results, eagerly. If we paginate it only needs to return a page's worth

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
     (def !cv (atom :alice))
     (def discard (p/run (try (binding [dom/node (dom/by-id "root")]
                                (tap [:typeahead-returned
                                      (typeahead (p/watch !cv)
                                        (p/fn [search]
                                          (tap [:query search])
                                          (p/for [e (q search)]
                                            (tap [:render e])
                                            (typeahead-item e
                                              (with (elem "div") (dom/text (get -data e))))))
                                        (p/fn [e] (get -data e))
                                        (reset! tphd dom/node))]))
                              (catch Pending _)
                              (catch Cancelled _)
                              (catch :default e (prn e)))))
     % := [:typeahead-returned :alice]

     "Search for C, pick Charlie D"
     (uit/focus @tphd)
     % := [:query "Alice B"]
     % := [:render :alice]
     (assert-visible-items-are [(:alice -data)])
     (uit/set-value! @tphd "C")
     % := [:query "C"]
     (hash-set % %) := #{[:render :bob] [:render :charlie]}
     (assert-visible-items-are [(:bob -data) (:charlie -data)])
     (uit/click (picklist-item "Charlie D"))
     % := [:query "Charlie D"]
     % := [:typeahead-returned :charlie]
     (count (picklist-items)) := 0
     (.-value @tphd) := "Charlie D"

     "Search for empty string, pick Derek B"
     (uit/focus @tphd)
     % := [:query "Charlie D"]
     % := [:render :charlie]
     (uit/set-value! @tphd "")
     % := [:query ""]
     (hash-set % % %) := #{[:render :alice] [:render :bob] #_[:render :charlie] [:render :derek]}
     (assert-visible-items-are (vals -data))
     (uit/click (picklist-item "Derek B"))
     % := [:query "Derek B"]
     % := [:typeahead-returned :derek]

     "Get new controlled value for :bob"
     (reset! !cv :bob)
     % := [:typeahead-returned :bob]
     (.-value @tphd) := "Bob C"

     "Don't get new controlled value while focused"
     (uit/focus @tphd)
     % := [:query "Bob C"]
     % := [:render :bob]
     (reset! !cv :alice)
     ;; TODO these shouldn't pass
     % := [:query "Alice B"]
     % := [:render :alice]
     % := [:typeahead-returned :alice]

     "Keyboard nav"
     ;; (uit/focus @tphd)
     ;; (uit/set-value! @tphd "")
     ;; (uit/press @tphd "Enter")
     ;; % := :alice
     ;; (.-value @tphd) := "Alice B"
     (discard)
     ))
