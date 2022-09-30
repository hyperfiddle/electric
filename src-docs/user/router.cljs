(ns user.router
  (:require [hyperfiddle.rcf :as rcf :refer [% tests with tap]]
            [hyperfiddle.event :as event]
            [missionary.core :as m]))

(def route-event-type "hf-routed")
(defn current-path [] (-> js/window .-location .-pathname))
(defn ->route-event ([] (->route-event (current-path))) ([path] (event/->event route-event-type path)))
(defonce popstate-listener
  (event/listen js/window "popstate" (fn [_] (event/dispatch js/window (->route-event)))))
(defn route! [path]
  (.pushState js/window.history nil "" path) (event/dispatch js/window (->route-event path)))
(defn reroute! [path]
  (.replaceState js/window.history nil "" path) (event/dispatch js/window (->route-event path)))
(defn from
  ([match-fn] (from match-fn current-path))
  ([match-fn init-fn]
   (->> (m/observe
          (fn mount [!]
            (let [discard (event/listen js/window route-event-type (comp ! event/data))]
              (! (init-fn))
              (fn unmount [] (discard)))))
     (m/eduction (keep match-fn) (dedupe)))))

(tests "basic routing works"
  (let [router (from {"/" :slash "/foo" :foo} (constantly "/foo")), it (router #(tap ::notified) #(do))]
    (with it                   % := ::notified  @it := :foo
      (route! "/")             % := ::notified  @it := :slash
      (route! "/foo")          % := ::notified  @it := :foo
      (reroute! "/")           % := ::notified  @it := :slash)))

(tests "values are deduped"
  (let [router (from {"/" :slash "/foo" :foo} (constantly "/foo")), it (router #(tap ::notified) #(do))]
    (with it                   % := ::notified  @it := :foo
      (route! "/")             % := ::notified  @it := :slash
      (route! "/")                      ; deduped
      (route! "/foo")          % := ::notified  @it := :foo)))

(tests "nil is discarded"
  (let [router (from {"/" :slash "/foo" :foo} (constantly "/foo")), it (router #(tap ::notified) #(do))]
    (with it                   % := ::notified  @it := :foo
      (route! "/")             % := ::notified  @it := :slash
      (route! "/doesnt-exist")          ; nil is discarded
      (route! "/foo")          % := ::notified  @it := :foo)))

(tests "a nil in-between same routes still dedupes"
  (let [router (from {"/" :slash "/foo" :foo} (constantly "/foo")), it (router #(tap ::notified) #(do))]
    (with it                   % := ::notified  @it := :foo
      (route! "/")             % := ::notified  @it := :slash
      (route! "/doesnt-exist")          ; nil is discarded
      (route! "/")                      ; deduped
      (route! "/foo")          % := ::notified  @it := :foo)))

(tests "first route goes through match-fn"
  (let [router (from tap), it (router #(do) #(do))]
    (with it @it := %)))

(tests "going back and forward triggers routing"
  (let [router (from {"/" :slash "/foo" :foo} (constantly "/foo")), it (router #(tap ::notified) #(do))]
    (with it                       % := ::notified  @it := :foo
      (route! "/")                 % := ::notified  @it := :slash
      (route! "/foo")              % := ::notified  @it := :foo
      (.back js/window.history)    % := ::notified  @it := :slash
      (.forward js/window.history) % := ::notified  @it := :foo)))
