(comment

  ; A Hyperfiddle request looks something like this
  ; /submission-masterlist/'needle'/tempida@$
  {:route   `(submission-masterlist "needle" "tempida@$")
   :tx      [:db/add ...]
   :basis-t *latest*}

  ; Which routes to an "endpoint" (just a fn) like this
  (defn ^:hyperfiddle.api/fiddle submission-masterlist [needle]
    (hf-eval hf/*$*
      ; Hyperfiddle DSL has reactive points, it is enhanced Datomic pull
      ; and returns a trace of the evaluation, a map of AST to evaluated AST
      [{`(submissions ~needle) [{:dustingetz/gender
                                 [:db/ident
                                  {`(shirt-sizes ~gender) [*]}]}]}
       {`(genders) [*]}]))

  ; It evaluates to something like this
  ; Each entry is a reactive seam in the overall fabric of the computation
  :=
  '{`$                               {:tempids ... :schema ...}
    `$'                              {:tempids ... :schema ...}

    `(genders ~`$)                   [#:db{:ident :scratch/male}
                                      #:db{:ident :scratch/female}]

    `(shirt-sizes ~`$ :scratch/male) [#:db{:ident :scratch/mens-small}
                                      #:db{:ident :scratch/mens-medium}
                                      #:db{:ident :scratch/mens-large}]

    `(submissions ~`$ "bob")         [#:scratch{:email  "bob@example.com",
                                                :gender {:db/ident    :scratch/male,
                                                         'shirt-sizes ~`(shirt-sizes $ :scratch/male)}}]}

  ; The UI attaches to this initial result and then can selectively
  ; reflow at these join points by naming the sexpression
  ; Thus you can implement a typeahead picker that reflows the `(submissions $ ~needle) query as 'needle is typed


  ; factor out cardinality for now, cardinality-many results need to be list-monad aware or something
  ~(let [needle (pure "bob")                                ; user input into form
         tx (pure [[:db/add ...]])                          ; primary staging area
         tx' (pure [[:db/retract ...]])                     ; consequent staging area

         $ (->> (hf/db "$" basis-t) (d/with ~tx))
         $' (->> (hf/db "$" basis-t) (d/with ~tx) (d/with ~tx'))

         record (-> ~(submissions $ ~needle) (d/pull [{:dustingetz/gender [:db/ident]}]))
         gender (:db/ident ~(:dustingetz/gender ~record))]

     ; Server evaluates the reactive computation while tracing the join points
     ; Which is exactly what the UI needs so the UI can reflow a specific subtree

     {`$                         ~$
      `$'                        ~$'
      `(submissions ~`$ ~needle) ~record
      `(shirt-sizes ~`$ ~gender) ~(shirt-sizes ~$ ~gender [*])
      `(genders $)               ~(genders ~$ [*])} #_(->> (genders) (d/pull-many ['*])))

  )

; Monad stuff
; ~ is "await", for example promises, these are all equivalent (untested)
; Functor laws let us rewrite (f ~(g ~x)) to ((comp f g) ~x)
(-> record (p/then :dustingetz/gender) (p/then :db/ident) (p/then shirt-sizes))
(-> record (p/then (comp :dustingetz/gender :db/ident shirt-sizes)))
(fmap shirt-sizes (fmap :db/ident (fmap :dustingetz/gender record)))
(fmap shirt-sizes (fmap (comp :db/ident :dustingetz/gender) record))
(shirt-sizes ~((comp :db/ident :dustingetz/gender) ~gender))
(shirt-sizes ~(:db/ident ~(:dustingetz/gender ~gender)))
(shirt-sizes ~((comp :db/ident :dustingetz/gender) ~gender))
