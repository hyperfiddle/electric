;;; Beware!
;;; Pushy internally creates a `goog.History` instance, which has many drawbacks.
;;; Please read the doc here https://google.github.io/closure-library/api/goog.History.html
;;; and make sure you understand this sentence:
;;; > This object should be created from a script in the document body before the
;;; > document has finished loading.
;;; It's normal and expected for this namespace to not play well with live reloading.
;;; If you want to make sure its state is in sync with the code, just hit refresh.

(ns hyperfiddle.client.router
  (:require [bidi.bidi :as bidi]
            [pushy.core :as pushy]
            [hyperfiddle.common.routes :as routes :refer [ROUTES]]
            [hyperfiddle.common.ednish :as ednish]))

(defonce !route (atom nil))
(declare history)

(defn document-location!
  "get the root-relative URL as string for routing Getting this as a string is
  useful for unifying portable backend/frontend routing code"
  []
  (str js/document.location.pathname
       js/document.location.search
       js/document.location.hash))

(defn set-route-from-url! [{:keys [handler _route-params]}]
  (if (= ::routes/sexpr handler)
    (reset! !route (ednish/url-decode (document-location!)))
    (throw (ex-info "Unknown route handler" {:handler handler}))))

;; Need a page refresh to update.
(defonce ^:private history
  (pushy/pushy set-route-from-url! (partial bidi/match-route ROUTES)))

(defn set-route! [sexp]
  ;; TODO assert route is valid here
  (pushy/set-token! history (ednish/url-encode sexp)))

(pushy/start! history)
