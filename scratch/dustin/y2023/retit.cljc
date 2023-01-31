

(def router
  (rr/router
    [["/yam2" ::home]
     ["/yam2/e/:id" {:name       :ent
                     :parameters {:path {:id int?}}}]]
    {:compile rc/compile-request-coercers
     :data    {:coercion rss/coercion}}))

#?(:cljs
   (defn set-page-title! [route-match]
     (set! (.-title js/document)
           (-> route-match :data :name str))))

#?(:cljs
   (p/def re-router
     (->> (m/observe
            (fn [!]
              (rfe/start!
                router
                !
                {:use-fragment false})))
          (m/relieve {})
          new)))

(p/defn Main []
  (try
    (binding [dom/node (dom/by-id "root")]
      (let [{:as match :keys [data query-params path-params]} re-router]
        (binding [d/route-match match
                  d/route       (some-> data :name)]
          (set-page-title! match)
          (new views/App))))
    (catch Pending _)
    (catch Cancelled e (throw e))
    (catch :default err
      (js/console.error (str (ex-message err) "\n\n" (dbg/stack-trace p/trace)) err))))