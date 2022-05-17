(ns user.photon-4-webview2
  "Photon fullstack query/view composition with client/server transfer"
  (:require [clojure.string :as string]
            [datascript.core :as d]
            [hyperfiddle.logger :as log]
            [hyperfiddle.ui :as ui]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests ! % with]])
  #?(:cljs (:require-macros user.photon-4-webview2)))       ; see readme


(hyperfiddle.rcf/enable!)

#?(:clj (def conn (d/create-conn {:order/email {}})))
#?(:clj (d/transact! conn [{:order/email "alice@example.com" :order/gender :order/female}
                           {:order/email "bob@example.com" :order/gender :order/male}
                           {:order/email "charlie@example.com" :order/gender :order/male}]))

(defn includes-str? [v needle]
  (string/includes? (string/lower-case (str v))
                    (string/lower-case (str needle))))

(defn orders [db ?email]
  #?(:clj
     (sort
       (d/q '[:find [?e ...]
              :in $ ?needle :where
              [?e :order/email ?email]
              [(user.photon-4-webview2/includes-str? ?email ?needle)]]
            db (or ?email "")))))

#?(:clj
   (tests
     (orders @conn "") := ["alice@example.com"
                           "bob@example.com"
                           "charlie@example.com"]
     (orders @conn "alice") := ["alice@example.com"]))

(p/def db)

(p/defn View [state]
  (let [email (ui/Input. {} dom/target-value)]
    (dom/table
      (dom/for [x ~@(orders db email)]
        (dom/tr
          (dom/td (dom/text x))
          (dom/td (dom/text ~@(:order/email (d/entity db x))))
          (dom/td (dom/text ~@(:order/gender (d/entity db x)))))))))

(def !db #?(:clj (atom @conn)))                             ; Photon cljsbuild unable to resolve !db
(def !state #?(:cljs (atom {:email ""})))

(p/defn App []
  (binding [dom/parent (dom/by-id "root")]
    (let [state (p/watch !state)]
      ~@(binding [db (p/watch !db)]
          ~@(View. state)))))

(def main #?(:cljs (p/client (p/main (log/info "starting")
                                     (log/info (pr-str (App.)))))))

(comment
  #?(:clj (@(requiring-resolve 'devkit/main) :main `main))
  #?(:clj (d/transact conn [{:order/email "dan@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "erin@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "frank@example.com"}]))
  #?(:clj (reset! !db @conn))

  (shadow.cljs.devtools.api/repl :app)
  (type 1)
  (swap! !state assoc :email "bob")
  (swap! !state assoc :email "")
  )
