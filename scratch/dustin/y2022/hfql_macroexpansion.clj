(require '[hyperfiddle.photon :as p]
         '[hyperfiddle.api :as hf])

(p/defn render [>v & [props]]
  ; renderer decides with ~ which queries to realize (i.e. abstraction-safe infinite scroll)
  ; this default renderer forces the whole tree
  (let [v ~>v]
    (cond
      (map? v) (into {} (p/for [[k >v] v]
                          [k ~>v]))
      (vector? v) (p/for [row v] ~row)
      :else ...)))

(hfql [{(submissions "")
        [{(:dustingetz/shirt-size . {::hf/options (shirt-sizes gender)})
          [:db/id :db/ident]}
         {(:dustingetz/gender . {::hf/options (genders)})
          [:db/id (:db/ident . {::hf/as gender})]}]}
       {(genders)
        [:db/ident]}])

; HFQL macroexpansion to Photon:

(let [>submissions               #'(submissions "")
      >genders                   #'(genders)
      >dustingetz_gender_options >genders]
  (render
    #'{'(submissions "")
       #'(render
           #'(p/for [e ~>submissions]
               (let [>dustingetz_shirt-size         #'(nav e :dustingetz/shirt-size)
                     >db_ident_1                    #'(nav ~>dustingetz_shirt-size :db/ident)
                     >db_id_1                       #'(nav ~>dustingetz_shirt-size :db/id)
                     >dustingetz_gender             #'(nav e :dustingetz/gender)
                     >db_ident_2                    #'(nav ~>dustingetz_gender :db/ident)
                     >gender                        >db_ident_2
                     >db_id_2                       #'(nav ~>dustingetz_gender :db/id)
                     >dustingetz_shirt-size_options #'(shirt-sizes ~>gender)]
                 #'(render
                     #'{:dustingetz/shirt-size
                        #'(render
                            #'{:db/id    #'(render >db_id_1)
                               :db/ident #'(render >db_ident_1)}
                            {::hf/options #'(p/for [e ~>dustingetz_shirt-size_options]
                                              (let [>db_id    #'(nav e :db/id)
                                                    >db_ident #'(nav e :db/ident)]
                                                (render
                                                  #'{:db/id    #'(render >db_id)
                                                     :db/ident #'(render >db_ident)})))})

                        :dustingetz/gender
                        #'(render
                            #'{:db/id    #'(render >db_id_2)
                               :db/ident #'(render >db_ident_2)}
                            {::hf/options #'(p/for [e ~>dustingetz_gender_options]
                                              (let [>db_id    #'(nav e :db/id)
                                                    >db_ident #'(nav e :db/ident)]
                                                #'(render #'{:db/id    #'(render >db_id)
                                                             :db/ident #'(render >db_ident)})))})}))))
       '(genders)
       #'(render
           #'(p/for [e ~>genders]
               (let [>db_ident #'(nav e :db/ident)]
                 #'(render #'{:db/ident #'(render >db_ident)}))))}))


; If photon were lazy (unfortunately v1 is not)

(let [submissions               (submissions "")
      genders                   (genders)
      dustingetz_gender_options genders]
  (render
    {'(submissions "")
     (render
       (p/for [e submissions]
         (let [dustingetz_shirt-size         (nav e :dustingetz/shirt-size)
               db_ident_1                    (nav dustingetz_shirt-size :db/ident)
               db_id_1                       (nav dustingetz_shirt-size :db/id)
               dustingetz_gender             (nav e :dustingetz/gender)
               db_ident_2                    (nav dustingetz_gender :db/ident)
               gender                        db_ident_2
               db_id_2                       (nav dustingetz_gender :db/id)
               dustingetz_shirt-size_options (shirt-sizes gender)]
           (render
             {:dustingetz/shirt-size
              (render
                {:db/id    (render db_id_1)
                 :db/ident (render db_ident_1)}
                {::hf/options (p/for [e dustingetz_shirt-size_options]
                                (let [db_id    (nav e :db/id)
                                      db_ident (nav e :db/ident)]
                                  (render
                                    {:db/id    (render db_id)
                                     :db/ident (render db_ident)})))})

              :dustingetz/gender
              (render
                {:db/id    (render db_id_2)
                 :db/ident (render db_ident_2)}
                {::hf/options (p/for [e dustingetz_gender_options]
                                (let [db_id    (nav e :db/id)
                                      db_ident (nav e :db/ident)]
                                  (render {:db/id    (render db_id)
                                           :db/ident (render db_ident)})))})}))))
     '(genders)
     (render
       (p/for [e genders]
         (let [db_ident (nav e :db/ident)]
           (render {:db/ident (render db_ident)}))))}))