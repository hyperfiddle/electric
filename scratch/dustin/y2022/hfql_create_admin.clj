(def create-admin [admin name email]
  [nil
   (fn tx [reason-string]
     `[(suber2.web.cmd.create/create' ~admin :admin ~name ~email reason-string)])])

(s/fdef create-admin :args (s/cat :admin string? :name string? :email ::ui/email))

(p/defn f []
  (let [admin (tempid)]
    (hfql (popover (create-admin admin . .)))))






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
