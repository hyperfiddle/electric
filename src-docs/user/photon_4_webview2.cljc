(ns user.photon-4-webview2
  "Photon fullstack query/view composition with client/server transfer"
  (:require [datascript.core :as d]                         ; photon cljsbuild needs to see the vars, fixme
            [hyperfiddle.ui :as ui]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests ! % with]]
            user devkit)
  #?(:cljs (:require-macros user.photon-4-webview2)))       ; see readme


(hyperfiddle.rcf/enable!)

#?(:clj (def conn (d/create-conn {:order/email {}})))
#?(:clj (d/transact! conn [{:order/email "alice@example.com" :order/gender :order/female}
                           {:order/email "bob@example.com" :order/gender :order/male}
                           {:order/email "charlie@example.com" :order/gender :order/male}]))

(defn orders [db ?email]
  #?(:clj
     (sort
       (d/q '[:find [?e ...]
              :in $ ?needle :where
              [?e :order/email ?email]
              [(user/includes-str? ?email ?needle)]]
            db (or ?email "")))))

#?(:clj
   (tests
     (orders @conn "") := [1 2 3]
     (orders @conn "alice") := [1]))

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

(def main #?(:cljs (p/client (p/main (App.)))))

(comment
  #?(:clj (devkit/main :main `main))
  #?(:clj (d/transact conn [{:order/email "dan@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "erin@example.com"}]))
  #?(:clj (d/transact conn [{:order/email "frank@example.com"}]))
  #?(:clj (reset! !db @conn))

  (shadow.cljs.devtools.api/repl :app)
  (type 1)
  (swap! !state assoc :email "bob")
  (swap! !state assoc :email "")
  )
