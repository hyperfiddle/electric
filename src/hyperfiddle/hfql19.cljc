(ns hyperfiddle.hfql19
  #?(:cljs (:require-macros [minitest :refer [tests]]))
  (:require
    [datascript.core :as d] #_#?(:clj [datomic.api :as d])
    [hyperfiddle.api :refer [*$*]]
    [hyperfiddle.incremental :refer [pureI fmapI bindI joinI sequenceI sequence-mapI capI]]
    [minitest #?@(:clj [:refer [tests]])]
    [meander.epsilon :as m]
    [missionary.core :refer [latest relieve watch ap ?!]]

    ; For inline tests, todo improve this
    [dustin.dev :refer [male female m-sm m-md m-lg w-sm w-md w-lg alice bob charlie]]
    [dustin.fiddle :refer [genders shirt-sizes submissions gender shirt-size submission]]))

; monad instance for Fabric values, which stack monads State and Incremental
; newtype Fabric = StateT (Map Sym Incremental a) Incremental b
; Monad m => StateT s m b
(defn pureF [a]
  (fn [s] (pureI [s a])))

(defn runS [SMa s]
  (SMa s))

;(defn runR [Rb a] (R/put Rb a)) ; nonsense

(defn bindF [SIa f]
  (fn [s]
    (bindI (runS SIa s) (fn [[s' a]]
                          (runS (f a) s')))))

(tests
  (def >z (runS (pureF 1) {}))
  (def !z (>z #(println :ready) #(println :done)))
  @!z := [{} 1]

  @((runS (pureF 1) {}) #() #())
  := [{} 1]

  (let [>z (runS
             (bindF (pureF 1) (fn [a]
                                (fn [s]
                                  #_(R/pure [s a])
                                  (bindI (get s '>c) (fn [c]
                                                       (let [b (+ a c)]
                                                         (pureI [s b])))))))
             {'>c (pureI 2)})]

    (second @(>z #() #())))
  ;:= [{'>c _} 3]
  := 3
  )

(defn fmapF [f & SIas]
  (fn [s]
    (let [Ias (->> SIas
                (map (fn [SIa]
                       (->> (runS SIa s)
                         (fmapI (fn [[s' a]]
                                  a))))))]
      (apply fmapI
        (fn [& as]
          [s (apply f as)])
        Ias))))

(tests
  @((runS (fmapF + (pureF 1) (pureF 2)) {}) #() #()) := [{} 3]
  @((runS (fmapF vector (pureF 1) (pureF 2)) {}) #() #()) := [{} [1 2]]
  @((runS (fmapF apply (pureF +) (pureF [1 2])) {}) #() #()) := [{} 3]
  @((runS (fmapF {:k 1} (pureF :k)) {}) #() #()) := [{} 1]
  @((runS (fmapF {} (pureF :k)) {}) #() #()) := [{} nil]

  ;@(R/cap (runS (fmapF identity (pureF alice)) {}))
  )

(defn sequenceF [mas]
  (apply fmapF vector mas))

(tests
  @((runS (sequenceF []) {}) #() #())
  := [{} []]
  @((runS (sequenceF [(pureF 1) (pureF 2)]) {}) #() #())
  := [{} [1 2]]

  @((runS (fmapF apply (pureF +) (sequenceF [(pureF 1) (pureF 2)])) {}) #() #())
  ;:= @((runS (fmapF #(apply % %&) (pureF +) (pureF 1) (pureF 2)) {}) #() #())
  := [{} 3]
  )

; ---

(defn hf-edge->sym [edge]
  (if (keyword? edge) (symbol edge) edge))

(tests
  (hf-edge->sym :dustingetz/gender) := 'dustingetz/gender
  (hf-edge->sym '>a) := '>a
  )

(defn parse-scoped-sym [k]
  (m/match (mapv symbol (clojure.string/split (str k) #":"))
    [?k] ?k
    [!ks ...] !ks))

(tests
  (parse-scoped-sym 'needle) := 'needle
  ;(parse-scoped-sym ['needle]) := ['needle]
  (parse-scoped-sym 'needle:dustingetz/email) := '[needle dustingetz/email])

(defn get-scope [scope k]
  #_(println `(get-scope ~k ~scope))
  (m/match (parse-scoped-sym k)
    [!ks ...] (let [[k & ks] !ks]
                (get scope (cons k (map #(get-scope scope %) ks))))
    ?k (get scope ?k)))

(tests
  (get-scope {'needle 1} 'needle) := 1
  (get-scope {'needle 1} 'foo) := nil
  (get-scope {'name "alice" ['needle "alice"] 42} 'needle:name) := 42
  (get-scope {'foo/name "alice" ['needle "alice"] 42} 'needle:foo/name) := 42
  )

(defn get-scopeI
  "evaluates composite scope keys before resolving in scope"
  [scope k]
  (println `(get-scopeI ~k ~(keys scope)))
  (m/match (parse-scoped-sym k)
    [!ks ...] (let [[k & ks] !ks
                    k:ks (sequenceI (cons (pureI k) (map #(get-scopeI scope %) ks)))]
                (println 'k:ks (pr-str @(k:ks #() #())))
                (println 'scope (pr-str (keys scope)))
                (bindI k:ks scope))
    ?k (let [x (scope ?k)]
         (if (nil? x) (pureI nil) x))))

; Scopes need to be able to see sibling scopes, not just parent scopes
; anything reachable by this cardinality strata
; in other words, state is flattened by cardinality
; also, it is ordered. topo sort by cardinality.
; that's not even enough.
; So
; we have to plan the order that the pull is traversed
; traverse all ancestors before descending?
; really its a topo-sort

(tests
  @((get-scopeI {'needle (pureI 1)} 'needle) #() #()) := 1
  @((get-scopeI {'needle (pureI 1)} 'foo) #() #()) := nil
  @((get-scopeI {'foo/name         (pureI "alice")
                 ['needle "alice"] (pureI 42)}
      'needle:foo/name) #() #())
  := 42)

(defn resolve' [k]
  #?(:clj  (resolve k)
     :cljs nil))

(defn askF [k]
  (fn [scope]
    (let [>v (or (get-scopeI scope k)
               (if-let [v (resolve' k)] (pureI v))
               (pureI [::no-def-for k]))]                   ; allocate atom ?
      (bindI >v (fn [v]
                  (println 'askF k v)
                  ; if not error ?
                  (pureI [scope v]))))))

(tests
  (second @((runS (askF 'needle:foo/name)
              {'foo/name         (pureI "alice")
               ['needle "alice"] (pureI 42)})
            #() #()))
  := 42
  )

(tests
  (second @((runS (askF '>a) {'>a (pureI 1)}) #() #()))
  := 1

  (second @((runS (askF '>a) {'>a (fmapI identity (pureI 1))}) #() #()))
  := 1

  ;@(R/cap (R/bind (R/fmap identity (R/pure 1)) R/pure))
  ;@(R/cap (R/bind (R/pure 1) R/pure))

  ;(second @((runS (askF '>a) {}) #() #()))
  ;:= [::no-def-for '>a]
  )

;(defn setSR [k v]
;  (fn [s] (resolved [(assoc s k v) nil])))

(defn hf-nav [kf ref]
  #_(println 'hf-nav kf ref)
  ; emits smart refs
  (kf (d/entity hyperfiddle.api/*$* ref)))

(tests
  "datascript"
  (hf-nav :db/ident 3) := :dustingetz/mens-small
  (hf-nav :db/id 3) := 3
  ;(hf-nav identity [:dustingetz/email "alice@example.com"]) := #:db{:id 9}
  (hf-nav :db/id [:dustingetz/email "alice@example.com"]) := 9
  (hf-nav :dustingetz/gender [:dustingetz/email "alice@example.com"]) := :dustingetz/female
  )

(defn hf-apply [edge a]
  #_(println 'hf-apply edge a)
  (cond
    (keyword? edge) (pureF (hf-nav edge a))
    (seq? edge) (let [[f & args] edge]
                  (fmapF apply (askF f) (sequenceF (map askF args))))
    () (do (println "hf-eval unmatched edge: " edge)
           (pureF [::unmatched-edge edge]))))

(tests
  (hf-nav :dustingetz/gender bob) := :dustingetz/male

  @((runS (hf-apply :dustingetz/gender bob) {}) #() #())
  := [{} :dustingetz/male]

  ;@(R/cap (runS (hf-apply (:dustingetz/gender '>b) alice) {'>b (R/pure alice)}))
  ;:= [{} :dustingetz/male]

  (second @((runS (hf-apply '(inc >a) alice) {'>a  (pureI 1)
                                              'inc (pureI inc)}) #() #()))
  := 2

  (second @((runS (hf-apply '(inc >a) alice) {'>a  (fmapI identity (pureI 1))
                                              'inc (pureI inc)}) #() #()))
  := 2

  @((fmapI identity (pureI 1)) #() #()) := 1)

(defn hf-eval [edge Fa]
  (bindF Fa (fn [a] #_(println 'hf-eval edge a)
              (bindF (hf-apply edge a) (fn [b] #_(println 'hf-eval edge a := b)
                                         (fn [s] (pureI [(assoc s (hf-edge->sym edge) (pureI b)) b])))))))

(tests
  (second @((runS (hf-eval :dustingetz/gender (pureF bob)) {}) #() #()))
  := :dustingetz/male
  ;:= (second @((runS (hf-eval :dustingetz/gender (fmapF identity (pureF bob))) {}) #() #()))
  )

(defn hf-pull-F [pat Fa]
  #_(println 'hf-pull-F pat)
  (m/match pat
    [!pats ...]                                             ; TODO distinguish [:a [:x :as x] :b]
    (as-> Fa Fa
      (sequenceF (map (fn [pat] (hf-pull-F pat Fa)) !pats))
      (fmapF (fn [as] (apply merge as)) Fa))

    {& (m/seqable [?edge ?pat])}
    (as-> Fa Fa
      (hf-eval ?edge Fa)
      (bindF Fa (fn [a]
                  (if (sequential? a)                       ; :db.cardinality/many - [] or ()
                    (sequenceF (->> a (map #(hf-pull-F ?pat (pureF %)))))
                    (hf-pull-F ?pat (pureF a)))))
      (fmapF (fn [a] {?edge a}) Fa))

    ?leaf
    (as-> Fa Fa
      (hf-eval ?leaf Fa)
      (fmapF (fn [a] {?leaf a}) Fa))))

; instance MonadTrans (StateT s) where
;   lift c = StateT $ \s -> c >>= (\x -> return (x,s))

(defn liftIF
  "lift base monad value into transformed monad value"
  [Ra]
  (fn [s]
    (fmapI
      (fn [a] [s a])
      Ra)))

(tests
  ;(Ra->SRa (p/resolved 1)) := (fn [s] (p/resolved [s 1]))
  @((runS (liftIF (pureI 1)) {}) #() #())
  ;:= @((runS (fn [s] (pureI [s 1])) {}) #() #())
  ;:= @((pureI [{} 1]) #() #())
  := [{} 1]

  @((runS (liftIF (pureI alice)) {}) #() #())
  := @((runS (pureF alice) {}) #() #())
  )

(defn hf-pull [pat & [Ia s]]
  (let [s (or s {})
        Ia (or Ia (pureI nil))]
    (fmapI
      (fn [[s' b]] #_(pureI) b)                             ; extract
      (runS (hf-pull-F pat (liftIF Ia)) s))))

(tests
  @((runS (hf-pull-F :dustingetz/gender
            #_(pureF bob) (fmapF identity (pureF bob))
            #_(liftIF (pureI alice))) {}) #() #())
  := [{} #:dustingetz{:gender :dustingetz/male}]

  @((hf-pull :dustingetz/gender (pureI bob)) #() #())
  := #:dustingetz{:gender :dustingetz/male}

  @((hf-pull :db/ident (pureI bob)) #() #())                ; datascript db/ident issue
  ;:= #:db{:ident :dustingetz/mens-small}

  @((hf-pull [:db/ident] (pureI bob)) #() #())              ; datascript db/ident issue
  ;:= #:db{:ident :dustingetz/mens-small}

  @((hf-pull [:db/ident :db/id] (pureI bob)) #() #())       ; datascript db/ident issue
  ;:= #:db{:ident :dustingetz/mens-small :id bob}

  @((hf-pull [{:dustingetz/gender [:db/ident :db/id]}] (pureI bob)) #() #())
  := #:dustingetz{:gender #:db{:ident :dustingetz/male, :id male}}

  @((hf-pull '[(shirt-size dustingetz/gender)] (pureI nil)
      {'dustingetz/gender (pureI :dustingetz/male)
       'shirt-size        (pureI dustin.fiddle/shirt-size)}) #() #())
  := {'(shirt-size dustingetz/gender) m-sm}

  @((hf-pull '[{:dustingetz/gender [(shirt-size dustingetz/gender)]}]
      (pureI bob)
      {'shirt-size (pureI dustin.fiddle/shirt-size)}) #() #())
  := #:dustingetz{:gender {'(shirt-size dustingetz/gender) m-sm}}

  @((hf-pull '[{:dustingetz/gender [(shirt-sizes dustingetz/gender)]}]
      (pureI bob)
      {'shirt-sizes (pureI shirt-sizes)}) #() #())
  := #:dustingetz{:gender {'(shirt-sizes dustingetz/gender) [m-sm m-md m-lg]}}

  @((hf-pull '(shirt-sizes gender)
      (pureI nil)
      {'gender      (pureI :dustingetz/male)
       'shirt-sizes (pureI shirt-sizes)
       }) #() #())
  := '{(shirt-sizes gender) (3 4 5)}

  @((hf-pull '{(shirt-sizes gender) [:db/ident]}
      (pureI nil)
      {'gender      (pureI :dustingetz/male)                ;good
       ;'gender      (pureI male) ; broken
       ;'gender      (pureI (hf-nav identity male))
       'shirt-sizes (pureI shirt-sizes)
       }) #() #())
  := '{(shirt-sizes gender) [#:db{:ident :dustingetz/mens-small}
                             #:db{:ident :dustingetz/mens-medium}
                             #:db{:ident :dustingetz/mens-large}]}

  @((hf-pull '[{:dustingetz/gender [{(shirt-sizes dustingetz/gender) [:db/ident]}]}]
      (pureI bob)
      {'shirt-sizes (pureI dustin.fiddle/shirt-sizes)}) #() #())
  := '#:dustingetz{:gender {(shirt-sizes dustingetz/gender)
                            [#:db{:ident :dustingetz/mens-small}
                             #:db{:ident :dustingetz/mens-medium}
                             #:db{:ident :dustingetz/mens-large}]}}

  @((hf-pull
      '[:db/id
        :dustingetz/type
        {:dustingetz/gender
         [{(shirt-sizes dustingetz/gender >needle)
           [:db/ident
            :db/id]}]}]
      (pureI bob)
      {'>needle     (pureI "")
       'shirt-sizes (pureI shirt-sizes)}) #() #())
  ;:= {:db/id             bob,
  ;    :dustingetz/type   :dustingetz/shirt-size,            ; test broken in datascript - db/ident issue
  ;    :dustingetz/gender {'(shirt-sizes dustingetz/gender >needle)
  ;                        [#:db{:ident :dustingetz/mens-small, :id m-sm}
  ;                         #:db{:ident :dustingetz/mens-medium, :id m-md}
  ;                         #:db{:ident :dustingetz/mens-large, :id m-lg}]}}

  @((hf-pull '[{(submissions) [:dustingetz/email]}]
      (pureI nil)
      {'needle      (pureI "")
       'submissions (pureI submissions)}) #() #())
  := '{(submissions) [#:dustingetz{:email "alice@example.com"}
                      #:dustingetz{:email "bob@example.com"}
                      #:dustingetz{:email "charlie@example.com"}]}


  (comment
    ; currently throws
    "complex scopes"
    @((hf-pull
        '[{(submissions needle)
           [{:dustingetz/gender
             [:db/ident
              {(shirt-sizes dustingetz/gender needle:dustingetz/email)
               [:db/ident]}]}
            :dustingetz/email]}]
        (pureI nil)
        {'needle                       (pureI "alice")
         ['needle "alice@example.com"] "small"
         'submissions                  (pureI submissions)
         'shirt-sizes                  (pureI shirt-sizes)
         'genders                      (pureI genders)})
      #() #())
    := '{(submissions needle) [#:dustingetz{:email  "alice@example.com",
                                            :gender {:db/ident :dustingetz/female,
                                                     (shirt-sizes dustingetz/gender)
                                                               [#:db{:ident :dustingetz/womens-small}
                                                                #:db{:ident :dustingetz/womens-medium}
                                                                #:db{:ident :dustingetz/womens-large}]}}]})

  (def !needle (atom nil))
  (def >z1 (hf-pull
             '[{(submissions needle)
                [:dustingetz/email
                 {:dustingetz/gender
                  [:db/ident
                   {(shirt-sizes dustingetz/gender #_needle:submission)
                    [:db/ident]}]}]}
               {(genders) [:db/ident]}]
             (pureI nil)
             {'needle      (watch !needle)
              'submissions (pureI submissions)
              'shirt-sizes (pureI shirt-sizes)
              'genders     (pureI genders)}))

  (def !z1 (>z1 #(println :ready) #(println :done)))
  @!z1 := '{(submissions needle) [#:dustingetz{:email  "alice@example.com",
                                               :gender {:db/ident                       :dustingetz/female,
                                                        (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/womens-small}
                                                                                         #:db{:ident :dustingetz/womens-medium}
                                                                                         #:db{:ident :dustingetz/womens-large}]}}
                                  #:dustingetz{:email  "bob@example.com",
                                               :gender {:db/ident                       :dustingetz/male,
                                                        (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                                         #:db{:ident :dustingetz/mens-medium}
                                                                                         #:db{:ident :dustingetz/mens-large}]}}
                                  #:dustingetz{:email  "charlie@example.com",
                                               :gender {:db/ident                       :dustingetz/male,
                                                        (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                                         #:db{:ident :dustingetz/mens-medium}
                                                                                         #:db{:ident :dustingetz/mens-large}]}}],
            (genders)            [#:db{:ident :dustingetz/male} #:db{:ident :dustingetz/female}]}

  (reset! !needle "alice")
  @!z1 := '{(submissions needle) [#:dustingetz{:email  "alice@example.com",
                                               :gender {:db/ident                       :dustingetz/female,
                                                        (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/womens-small}
                                                                                         #:db{:ident :dustingetz/womens-medium}
                                                                                         #:db{:ident :dustingetz/womens-large}]}}],
            (genders)            [#:db{:ident :dustingetz/male} #:db{:ident :dustingetz/female}]}

  ; these should not perform any work, but currently do (add a println in shirt-sizes to see)
  ; due to emulation of continuous bind with discrete primitives
  (reset! !needle "alice")
  (reset! !needle "bob")
  (reset! !needle "charlie")
  (reset! !needle "zetta")
  (reset! !needle "e@example.com")
  @!z1 := '{(submissions needle) [#:dustingetz{:email  "alice@example.com",
                                               :gender {:db/ident                       :dustingetz/female,
                                                        (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/womens-small}
                                                                                         #:db{:ident :dustingetz/womens-medium}
                                                                                         #:db{:ident :dustingetz/womens-large}]}}
                                  #:dustingetz{:email  "charlie@example.com",
                                               :gender {:db/ident                       :dustingetz/male,
                                                        (shirt-sizes dustingetz/gender) [#:db{:ident :dustingetz/mens-small}
                                                                                         #:db{:ident :dustingetz/mens-medium}
                                                                                         #:db{:ident :dustingetz/mens-large}]}}],
            (genders)            [#:db{:ident :dustingetz/male} #:db{:ident :dustingetz/female}]}

  )
