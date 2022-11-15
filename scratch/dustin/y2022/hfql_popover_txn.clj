; new qual popover
[{:db/id "tempid" :sub-attr/keyword "permit/ca" ...}
 [:db/add "edward" :sub/qual "tempid"]]

(def create-qual [sub]
  (let [e] (tempid!)
    [e
     (fn tx []
       `[[:db/add ~sub :sub/qual ~e]])]))

(s/fdef create-qual :args (s/cat :sub ref?))

(p/defn App [sub]
  (hfql {(popover (create-qual sub))
         [:sub-attr/keyword
          :sub-attr/name
          ...]}))

; create admin popover
(def create-admin [admin name email]
  [nil
   (fn tx [reason-string]
     `[(suber2.web.cmd.create/create' ~admin :admin ~name ~email reason-string)])])

(s/fdef create-admin :args (s/cat :admin string? :name string? :email ::ui/email))

(p/defn f []
  (let [admin (tempid)]
    (hfql (popover (create-admin admin . .)))))










; previous iteration

(def create-admin-query [admin name email] nil)

(def create-admin-tx [admin name email]
  `[(suber2.web.cmd.create/create' ~admin :admin ~name ~email)])

(s/fdef create-admin-query :args (s/cat :admin string? :name string? :email ::ui/email))
(s/fdef create-admin-tx    :args (s/cat :admin string? :name string? :email ::ui/email))
(p/defn f []
  (let [admin (tempid)]

    (hfql (popover (create-admin-tx admin name email)))


    (hfql (popover (create-admin-query admin . .)
                   {::hf/tx (create-admin-tx admin name email)}))

    (hfql {(popover (create-admin-query admin . .)
                    {::hf/tx (create-admin-tx admin name email)})
           []})))



(p/defn SubRenamePopoverBody []
  (hfql [:sub/name]))

(p/defn Page []
  (binding [hf/enity edward]
    (Popover. WithStage SubRenamePopoverBody)))
