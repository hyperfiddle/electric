; request = [sexp, t]
; t ~ stage

; What is a pid?
; what is a ctx?
; ctx = pid = focused scope in view
; pid = result + leaf

(comment
  ; server utility impl
  (defn hf-db [dbname parent-t tx]
    (-> (hf/db dbname *conn*) (d/as-of parent-t) (d/with tx)))


  ; What is the server app classpath
  (defn ^::hf/fiddle submission-masterlist [needle]
    (hf-eval hf/*$*
      [{`(submissions ~needle) [{:dustingetz/gender
                                 [:db/ident
                                  {`(shirt-sizes ~gender) [*]}]}]}
       {`(genders) [*]}]))

  ; what is the route
  ; /submission-masterlist/'needle'/tempida@$
  {:route   (submission-masterlist "needle" #_"tempida@$")
   :tx      [:db/add ...]
   :basis-t *latest*}

  ; What is the request expanded
  (binding [hf/*$* (-> (d/db *conn*) (d/as-of *latest*) (d/with [[:db/add ...]]))]
    #_(submission-masterlist "needle")
    (hf-eval-pull hf/*$*
      [{`(submissions ~needle) [{:dustingetz/gender
                                 [:db/ident
                                  {`(shirt-sizes ~gender) [*]}]}]}
       {`(genders) [*]}]))

  ; factor out cardinality for now
  ; This is the reactive AST
  ~(let [tx (pure [[:db/add ...]])
         $ ~(hf/db "$" parent-t ~tx)
         tx' (pure [[:db/retract ...]])
         $' ~(d/with ~$ ~tx')
         $users ~(hf/db "$users" parent-t ~tx)
         needle (pure "bob")
         x (-> (submissions ~needle) (d/pull [{:dustingetz/gender [:db/ident]}]))
         gender (:db/ident ~(:dustingetz/gender ~x))]
     {`$                       ~$
      `$users                  ~$users
      `(submissions $ ~needle) ~x
      `(shirt-sizes $ ~gender) ~(shirt-sizes ~gender [*])
      `(genders $ $users)      ~(genders [*])} #_(->> (genders) (d/pull-many ['*])))


  ; What does the server send back
  =>
  {`$      {:tempids ... :schema ...}
   `$'     {:tempids ... :schema ...}
   `$users {:tempids ... :schema ...}

   `(genders ~`(hf/db "$" parent-t tx) hf/*$users*)
           [#:db{:ident :scratch/male}
            #:db{:ident :scratch/female}]

   `(shirt-sizes hf/*$* :scratch/male)
           [#:db{:ident :scratch/mens-small}
            #:db{:ident :scratch/mens-medium}
            #:db{:ident :scratch/mens-large}]

   `(submissions hf/*$* "bob")
           [#:scratch{:email  "bob@example.com",
                      :gender {:db/ident    :scratch/male,
                               'shirt-sizes ~`(shirt-sizes hf/*$* :scratch/male)}}]}

  )




(shirt-sizes (fmap (comp :db/ident :dustingetz/gender) gender))

(shirt-sizes (p/then gender (comp :db/ident :dustingetz/gender)))
(shirt-sizes (p/then gender (comp :db/ident :dustingetz/gender)))

(-> gender (p/then :dustingetz/gender) (p/then :db/ident) (p/then shirt-sizes))
(-> gender (p/then (comp :dustingetz/gender :db/ident shirt-sizes)))
(shirt-sizes ((comp :db/ident :dustingetz/gender) ~gender))
(shirt-sizes ~(:db/ident ~(:dustingetz/gender ~gender)))
(shirt-sizes ((comp :db/ident :dustingetz/gender) ~gender))


[`(f "a") 1234]
[`(f "b") 1234]
[`(f "c") 1234]

; How is the time encoded ito the pid?

(def *latest* "root")

; request is of form
; /shirt-sizes/:male implies *latest* aka "root"
{[`(shirt-sizes :male) *latest*]
 {:result       [::small ::medium]
  :db-with-meta {:tempids []}
  :schema       []}
 [`(shirt-sizes :male) (+ 1 *latest*)]
 [::small ::medum ::new-size]}

; Coordinating time relative to *latest* makes it stable in the view

; For the client-server interface, don't need pid stuff to coordinate.
; need sexp + relative time.

(hf-eval `(shirt-sizes :male) (+ 1 *latest*))

(hf-eval [sexp, $])