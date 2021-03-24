(ns dustin.free-toylang)


(comment


  (M (M x))
  (M x)

  (deftype IdentityM []
    Do-via
    (inject [_]
      {'bind   (fn [mv mf] (mf mv))
       'return ...
       'fapply ...
       'fmap   ...
       'throw  (fn [x]
                 [nil x]
                 )}))


  ((via (eval-scope)
     '(let [a 1
            b (+ a 2)]
        (dec (inc b))))
   {'c 42})
  :=


  (macroexpand (via (IdentityM.)
                 (let [a 1
                       b (+ a 2)]
                   (dec (inc b)))))
  :=
  (bind 1         (fn [a]
                    (bind (+ a 2)   (fn [b]
                                      (return (dec (inc b)))))))

  (def ast '(bind 1 (fn [a]
                      (bind (+ a 2) (fn [b]
                                      (return (dec (inc b))))))))
  (letfn [(bind ...)
          (return ...)]
    (eval ast))

  ; Command language
  '(Bell next)
  '(Output b next)
  '(Done)

  (deftype BellHandlers
    Do-via
    (inject [_]
      {'bell   (fn [next] (println "bell") (next))            ; :: IO
       'output (fn [b next] (println b) (next))
       'done   (fn [])}))

  (via (BellHandlers.)
    '(let [_ (bell)
           _ (output "A")]
       (done)))

  '{(submission needle) [{:dustingetz/gender
                          [:db/ident
                           {(shirt-size dustingetz/gender >needle2) [*]}]}]}

  (d/pull $ [::name
             {::gender
              [::a
               :db/ident]}] 12345)
  := {::name "dustin"
      ::gender {::a nil
                :db/ident ::male}}

  (defn f [v] (:db/ident v))

  (via (stream)
    [::name
     {::gender
      [::a
       shirt-size
       #_(shirt-size gender)]}])

  (-> (d/entity 12345) ::gender shirt-size) := ::male
  (-> (d/entity 12345) ::name) := "dustin"
  (-> (d/entity 12345) ::gender ::a) := nil



  )