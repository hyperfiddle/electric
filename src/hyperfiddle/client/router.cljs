;;; * Beware!
;;;
;;;   Pushy internally creates a `goog.History` instance, which has many
;;;   drawbacks. Please read the doc [[https://google.github.io/closure-library/api/goog.History.html][here]] and make sure you understand this
;;;   sentence:
;;;
;;;   #+begin_quote
;;;   This object should be created from a script in the document
;;;   body before the document has finished loading.
;;;   #+end_quote
;;;
;;;   It's expected this namespace to not play well with live reloading. If you
;;;   want to make sure its state is in sync with the code, just hit refresh.

(ns hyperfiddle.client.router
  (:require [bidi.bidi :as bidi]
            [pushy.core :as pushy]
            [hyperfiddle.common.routes :as routes :refer [ROUTES]]
            [hyperfiddle.common.ednish :as ednish]))

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
    (reset! routes/!route (ednish/url-decode (document-location!)))
    (throw (ex-info "Unknown route handler" {:handler handler}))))

;; Need a page refresh to update.
(defonce ^:private history
  (pushy/pushy set-route-from-url! (partial bidi/match-route ROUTES)))

(defn set-route! [sexp]
  (when (seq? sexp)
    (when-some [route (seq sexp)]
      (pushy/set-token! history (ednish/url-encode route)))))

(pushy/start! history)
