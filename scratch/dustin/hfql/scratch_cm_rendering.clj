{(dustin.fiddle/submission "alice")
 {:dustingetz/gender {:db/ident :dustingetz/female},
  :db/id             #hyperfiddle.api.Link{:href  (dustin.fiddle-pages/page-submission-details 9), :value 9}},
 (dustin.fiddle/gender)
 {:db/ident :dustingetz/male}}


; next state
{(dustin.fiddle/submission "alice")
 {:dustingetz/gender #autocomplete {:selection {:db/ident :dustingetz/male} :options []},
  :db/id             #link {:href (dustin.fiddle-pages/page-submission-details 9), :value 9}},
 (dustin.fiddle/gender)
 {:db/ident :dustingetz/male}}


; tree diff - of database state before and after
[[:db/retract 9 :dustingetz/gender {:db/ident :dustingetz/female}]
 [:db/add 9 :dustingetz/gender {:db/ident :dustingetz/male}]]


; problem is hovering the input prevents serializing closure through edn values at codemirror root? NO

; even if we have fine grained effects, you don't have a way for codemirror to call back into the code in order to set the needle






{(dustin.fiddle/submission "alice") {:dustingetz/gender [:div
                                                         #hyperfiddle.api.Input{:id #uuid "5cb7f888-10da-4432-a427-1f2a56a14717",
                                                                                :value ""}
                                                         [:dustin.fiddle-pages/selection
                                                          {:db/ident :dustingetz/female}]
                                                         [:dustin.fiddle-pages/options
                                                          6]],
                                     :dustingetz/email [:dustin.fiddle-pages/hi
                                                        "9"],
                                     :db/id #hyperfiddle.api.Link{:href (dustin.fiddle-pages/page-submission-details
                                                                          9),
                                                                  :value 9}},
 (dustin.fiddle/gender) {:db/ident :dustingetz/male}}