(ns hyperfiddle.router-html5
  (:require [hyperfiddle.router :as router]
            [hyperfiddle.photon :as p]
            [missionary.core :as m]))

#?(:cljs (defn pushState!    [path] (.pushState    js/window.history nil "" path)))
#?(:cljs (defn replaceState! [path] (.replaceState js/window.history nil "" path)))
#?(:cljs (defn back!         []     (.back js/window.history)))
#?(:cljs (defn forward!      []     (.forward js/window.history)))

(defn notify-watches [this oldval]
  (let [newval (deref this)]
    (doseq [[key callback] (deref (:watches this))]
      (callback key this oldval newval))))

(def location (constantly #?(:clj nil :cljs (.-location js/window))))

(defn path [] (let [loc (location)]
                (str (.-pathname loc) (.-search loc) (.-hash loc))))

#?(:cljs
   (defrecord HTML5History [encode decode watches]
     IAtom
     ISwap
     (-swap! [this f]           (let [oldval (deref this)
                                      newval (f oldval)]
                                  (replaceState! (encode newval))
                                  (notify-watches this oldval)))
     (-swap! [this f arg]       (-swap! this #(f % arg)))
     (-swap! [this f arg1 arg2] (-swap! this #(f % arg1 arg2)))
     (-swap! [this f x y args]  (-swap! this #(apply f % x y args)))

     IReset
     (-reset! [this newval]     (-swap! this (constantly newval)))

     IWatchable
     (-add-watch [this key callback]
       (swap! watches assoc key callback)
       this)
     (-remove-watch [_ key] (swap! watches dissoc key))
     (-notify-watches [this oldval newval] (notify-watches this oldval))

     IDeref
     (-deref [_] (decode (path)))

     router/IHistory
     (navigate! [this route]
       (let [oldval (deref this)]
         (pushState! (encode route))
         (notify-watches this oldval)))
     (back! [this]
       (let [oldval (deref this)]
         (back!)
         (notify-watches this oldval)))
     (forward! [this]
       (let [oldval (deref this)]
         (notify-watches this oldval)))
     (replace-state! [this new-state]
       (.reset this new-state))

     ;; TODO Implement equality so that two HTML5History are always equal. The
     ;;      browser history is a singleton. There is no use case for nested
     ;;      HTML5History instances on the page.
     ))

(defn html5-history [encode decode] #?(:cljs (->HTML5History encode decode (atom {}))))

(p/defn HTML5-History []
  (let [history (html5-history router/encode router/decode)]
    (new (m/observe (fn [!]
                      (! nil)
                      (let [f (fn [_e] (-notify-watches history nil (deref history)))]
                        (.addEventListener js/window "popstate" f)
                        #(.removeEventListener js/window "popstate" f)))))
    history))
