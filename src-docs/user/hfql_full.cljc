(ns user.hfql-full
  (:require [hfdl.lang :as p]
            [hyperfiddle.api :as hf]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.rcf :refer [tests ! %]]
            [hyperfiddle.ui3 :as ui]
            [user.gender-shirt-size :refer [submissions genders shirt-sizes emails]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            #?(:clj [hyperfiddle.q6 :refer [hfql] :as hfql])
            )
  #?(:cljs (:require-macros [hyperfiddle.api :refer [render]]
                            [hyperfiddle.q6 :refer [hfql] :as hfql]
                            [user.hfql-full :refer [page-submissions page-submissions-query]])))

(s/fdef user-name :ret any?) ;; cardinality one
(p/def user-name #'(let [email ~(hf/nav hf/entity :dustingetz/email)]
                     (first (str/split email #"@"))))

(p/defn page-submissions-query []
  (let [sub 9]
    (hfql {(submissions {(needle . ::hf/options (emails needle)
                                 ::hf/option-label :dustingetz/email
                                 ;; ::hf/default (or needle sub) ;; Missing traverse on default value (TBD)
                                 )
                         [:dustingetz/email] ;; view defaults
                         })
           [#_'(constant-link sub) ;; FIXME hf/render not called
            #_'user-profile
            #_user-name
            :dustingetz/email ;; TODO ::hf/link property
            {(:dustingetz/gender . ::hf/options (genders)
                                 ::hf/option-label :db/ident
                                 ::hf/render ui/table-picker
                                 )
             [(:db/ident . ::hf/as gender)]}
            {(:dustingetz/shirt-size . ::hf/options (shirt-sizes gender needle) ;; TODO `%` meaning "value being typed" ?
                                     ::hf/option-label :db/ident
                                     ::hf/render ui/typeahead
                                     )
             [:db/ident]}]}
          ) 
    )
  )


(p/defn page-submissions []
  (p/$ ui/with-spec-render
       #'(binding [hf/render        ui/render ;; spec renderer
                   hf/link          ui/link   ;; render links as `<a href=…>`, should be bound by hf/router
                  hfql/render-mode ~ui/render-mode-selector]
           (prn "RENDER mode" hyperfiddle.q6/render-mode)
           (dom/div (dom/class "page-submissions")
                    (p/$ page-submissions-query)))))

(comment
  (hyperfiddle.rcf/enable!)
  (tests
    (p/run (binding [hfql/render-mode ::hfql/edn] (! (p/$ page-submissions-query))))
    % := '{(user.gender-shirt-size/submissions needle)
           [#:dustingetz{:gender #:db{:ident :dustingetz/female},
                         :email "alice@example.com",
                         :shirt-size #:db{:ident :dustingetz/womens-large}}
            #:dustingetz{:gender #:db{:ident :dustingetz/male},
                         :email "bob@example.com",
                         :shirt-size #:db{:ident :dustingetz/mens-large}}
            #:dustingetz{:gender #:db{:ident :dustingetz/male},
                         :email "charlie@example.com",
                         :shirt-size #:db{:ident :dustingetz/mens-medium}}]})
  )
